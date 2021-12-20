use crate::error::{Error, Result};
use std::convert::TryInto;
use std::slice::Iter;

const NOTE_OFF: u8 = 0x80;
const NOTE_ON: u8 = 0x90;
const POLY_AFTERTOUCH: u8 = 0xa0;
const CONTROL_CHANGE: u8 = 0xb0;
const PROGRAM_CHANGE: u8 = 0xc0;
const CHANNEL_AFTERTOUCH: u8 = 0xd0;
const PITCH_BEND: u8 = 0xe0;
const SYSEX: u8 = 0xf0;
const EOX: u8 = 0xf7;
const TRACK_NUMBER: u8 = 0x00;
const TEXT_EVENT: u8 = 0x01;
const COPYRIGHT_NOTICE: u8 = 0x02;
const TRACK_NAME: u8 = 0x03;
const INSTRUMENT_NAME: u8 = 0x04;
const LYRIC: u8 = 0x05;
const MARKER: u8 = 0x06;
const CUE_POINT: u8 = 0x07;
const MIDI_CHANNEL_PREFIX: u8 = 0x20;
const END_OF_TRACK: u8 = 0x2f;
const SET_TEMPO: u8 = 0x51;
const SMPTE_OFFSET: u8 = 0x54;
const TIME_SIGNATURE: u8 = 0x58;
const KEY_SIGNATURE: u8 = 0x59;
const SEQUENCER_SPECIFIC: u8 = 0x7f;

#[derive(Debug, Clone, Copy)]
pub struct Header(pub [u8; 14]);

impl Header {
    pub fn new(fmt: u16, ntrks: u16, div: u16) -> Result<Self> {
        match (fmt, ntrks, div) {
            (_, 0, _) => {}
            (_, _, 0) => return Err(Error::InvalidTimeDiv),
            (0, 1, _) => {}
            (0, _, _) => return Err(Error::InvalidFmt0),
            (1, _, _) => {}
            (2, _, _) => {}
            _ => return Err(Error::InvalidFormat),
        }

        let mut data = [
            0x4d, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];
        data[8..10].copy_from_slice(&fmt.to_be_bytes());
        data[10..12].copy_from_slice(&ntrks.to_be_bytes());
        data[12..14].copy_from_slice(&div.to_be_bytes());

        Ok(Self(data))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Event {
    NoteOff(u8, u8, u8),
    NoteOn(u8, u8, u8),
    PolyAftertouch(u8, u8, u8),
    ControlChange(u8, u8, u8),
    ProgramChange(u8, u8),
    ChannelAftertouch(u8, u8),
    PitchBend(u8, u8, u8),
    SysEx,
    TrackNumber,
    TextEvent,
    CopyrightNotice,
    TrackName,
    InstrumentName,
    Lyric,
    Marker,
    CuePoint,
    MidiChannelPrefix,
    EndOfTrack,
    SetTempo(u32),
    SmpteOffset,
    TimeSignature(u8, u8, u8, u8),
    KeySignature,
    SequencerSpecific,
}

pub type TrackEvent = (u32, Event);

pub struct Track {
    clock: u32,
    prev_t: u32,
    status: u8,
    queue: Vec<TrackEvent>,
    buf: Vec<u8>,
}

impl Track {
    pub fn new() -> Self {
        Self {
            clock: 0,
            prev_t: 0,
            status: 0,
            queue: Vec::new(),
            buf: vec![0x4d, 0x54, 0x72, 0x6b, 0, 0, 0, 0],
        }
    }

    pub fn advance(&mut self, delta_t: u32) -> Result<()> {
        self.clock = self.clock.checked_add(delta_t).ok_or(Error::Overflow)?;

        while let Some((time, event)) = self.queue.last().filter(|t| t.0 <= self.clock).copied() {
            self.queue.pop().unwrap();
            self.write_event(time, event);
        }

        Ok(())
    }

    pub fn finish(&mut self) -> Result<&[u8]> {
        while !self.queue.is_empty() {
            self.advance(96)?;
        }

        self.put_event(0, Event::EndOfTrack)?;
        self.update_len()?;
        Ok(&self.buf)
    }

    pub fn is_empty(&self) -> bool {
        self.buf.len() <= 8
    }

    pub fn put_event(&mut self, delta_t: u32, event: Event) -> Result<()> {
        if delta_t == 0 {
            self.write_event(self.clock, event);
        } else {
            let time = self.clock.checked_add(delta_t).ok_or(Error::Overflow)?;
            let idx = self
                .queue
                .iter()
                .position(|t| t.0 <= time)
                .unwrap_or_else(|| self.queue.len());
            self.queue.insert(idx, (time, event));
        }
        Ok(())
    }

    fn update_len(&mut self) -> Result<()> {
        let len: u32 = (self.buf.len() - 8)
            .try_into()
            .map_err(|_| Error::TrackTooLong)?;
        let b = len.to_be_bytes();
        self.buf[4..8].copy_from_slice(&b);
        Ok(())
    }

    fn write_event(&mut self, time: u32, event: Event) {
        let delta_t = time - self.prev_t;
        self.buf.write_var_len(delta_t);
        self.prev_t = time;
        use Event::*;

        match event {
            NoteOff(chn, num, vel) => self.cvm(&[NOTE_OFF | chn, num, vel]),
            NoteOn(chn, num, vel) => self.cvm(&[NOTE_ON | chn, num, vel]),
            PolyAftertouch(chn, num, val) => self.cvm(&[POLY_AFTERTOUCH | chn, num, val]),
            ControlChange(chn, id, val) => self.cvm(&[CONTROL_CHANGE | chn, id, val]),
            ProgramChange(chn, prg) => self.cvm(&[PROGRAM_CHANGE | chn, prg]),
            ChannelAftertouch(chn, val) => self.cvm(&[CHANNEL_AFTERTOUCH | chn, val]),
            PitchBend(chn, lsb, msb) => self.cvm(&[PITCH_BEND | chn, lsb, msb]),
            EndOfTrack => self.meta(END_OF_TRACK, &[]),
            SetTempo(tempo) => self.meta(SET_TEMPO, &tempo.to_be_bytes()[1..]),
            TimeSignature(nn, dd, cc, bb) => self.meta(TIME_SIGNATURE, &[nn, dd, cc, bb]),
            _ => todo!(),
        }
    }

    fn write(&mut self, b: &[u8]) {
        self.buf.extend(b.iter());
    }

    fn cvm(&mut self, b: &[u8]) {
        if b[0] == self.status {
            self.write(&b[1..]);
        } else {
            self.write(b);
            self.status = b[0];
        }
    }

    fn meta(&mut self, id: u8, data: &[u8]) {
        self.write(&[0xff, id]);
        self.buf.write_var_len(data.len() as u32);
        self.write(data);
        self.status = 0;
    }
}

pub trait WriteVarLen {
    fn write_var_len(&mut self, val: u32);
}

pub trait ReadVarLen {
    fn read_var_len(&mut self) -> u32;
}

impl WriteVarLen for Vec<u8> {
    fn write_var_len(&mut self, val: u32) {
        let mut offset = match val {
            0..=0x7f => 0,
            0x80..=0x3fff => 1,
            0x4000..=0x1fffff => 2,
            _ => 3,
        };

        while offset != 0 {
            let shift_by = offset * 7;
            let u7 = ((val >> shift_by) & 0x7f) as u8;
            self.push(u7 | 0x80);
            offset -= 1;
        }

        self.push((val & 0x7f) as u8);
    }
}

impl ReadVarLen for Iter<'_, u8> {
    fn read_var_len(&mut self) -> u32 {
        let mut byte = *self.next().expect("BUG: unexpected end of chunk");
        let mut val = (byte & 0x7f) as u32;

        while byte & 0x80 != 0 {
            val <<= 7;
            byte = *self.next().expect("BUG: unexpected end of chunk");
            val += (byte & 0x7f) as u32;
        }

        val
    }
}

#[cfg(test)]
pub mod tests {
    use super::{Event, Header, ReadVarLen, Track, WriteVarLen};

    pub const FILE0: &[u8] = &[
        0x4d, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x01, 0x00, 0x60, 0x4d,
        0x54, 0x72, 0x6b, 0x00, 0x00, 0x00, 0x3b, 0x00, 0xff, 0x58, 0x04, 0x04, 0x02, 0x18, 0x08,
        0x00, 0xff, 0x51, 0x03, 0x07, 0xa1, 0x20, 0x00, 0xc0, 0x05, 0x00, 0xc1, 0x2e, 0x00, 0xc2,
        0x46, 0x00, 0x92, 0x30, 0x60, 0x00, 0x3c, 0x60, 0x60, 0x91, 0x43, 0x40, 0x60, 0x90, 0x4c,
        0x20, 0x81, 0x40, 0x82, 0x30, 0x40, 0x00, 0x3c, 0x40, 0x00, 0x81, 0x43, 0x40, 0x00, 0x80,
        0x4c, 0x40, 0x00, 0xff, 0x2f, 0x00,
    ];

    pub const FILE1: &[u8] = &[
        0x4d, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x01, 0x00, 0x04, 0x00, 0x60, 0x4d,
        0x54, 0x72, 0x6b, 0x00, 0x00, 0x00, 0x14, 0x00, 0xff, 0x58, 0x04, 0x04, 0x02, 0x18, 0x08,
        0x00, 0xff, 0x51, 0x03, 0x07, 0xa1, 0x20, 0x83, 0x00, 0xff, 0x2f, 0x00, 0x4d, 0x54, 0x72,
        0x6b, 0x00, 0x00, 0x00, 0x10, 0x00, 0xc0, 0x05, 0x81, 0x40, 0x90, 0x4c, 0x20, 0x81, 0x40,
        0x4c, 0x00, 0x00, 0xff, 0x2f, 0x00, 0x4d, 0x54, 0x72, 0x6b, 0x00, 0x00, 0x00, 0x0f, 0x00,
        0xc1, 0x2e, 0x60, 0x91, 0x43, 0x40, 0x82, 0x20, 0x43, 0x00, 0x00, 0xff, 0x2f, 0x00, 0x4d,
        0x54, 0x72, 0x6b, 0x00, 0x00, 0x00, 0x15, 0x00, 0xc2, 0x46, 0x00, 0x92, 0x30, 0x60, 0x00,
        0x3c, 0x60, 0x83, 0x00, 0x30, 0x00, 0x00, 0x3c, 0x00, 0x00, 0xff, 0x2f, 0x00,
    ];

    const VAR_LEN: &[u8] = &[
        0x00, 0x40, 0x7f, 0x81, 0x00, 0xc0, 0x00, 0xff, 0x7f, 0x81, 0x80, 0x00, 0xc0, 0x80, 0x00,
        0xff, 0xff, 0x7f, 0x81, 0x80, 0x80, 0x00, 0xc0, 0x80, 0x80, 0x00, 0xff, 0xff, 0xff, 0x7f,
    ];

    pub fn bindiff(a: &[u8], b: &[u8]) {
        if a != b {
            let mut left = String::new();
            let mut right = String::new();

            for (l, r) in a.chunks(10).zip(b.chunks(10)) {
                for (i, lb) in l.iter().enumerate() {
                    left.push_str(&format!("{:02x} ", lb));
                    match r.get(i) {
                        Some(rb) if lb == rb && rb > &127 => {
                            right.push_str(&format!("\x1b[34m{:02x} ", rb))
                        }
                        Some(rb) if lb == rb => right.push_str(&format!("\x1b[32m{:02x} ", rb)),
                        Some(rb) if rb > &127 => right.push_str(&format!("\x1b[35m{:02x} ", rb)),
                        Some(rb) => right.push_str(&format!("\x1b[31m{:02x} ", rb)),
                        None => right.push_str(&"\x1b[31m-- "),
                    }
                }

                println!("{:<31}  {:<31}\x1b[0m", left, right);
                left.clear();
                right.clear();
            }

            panic!();
        }
    }

    #[test]
    fn write_var_len() {
        let mut v = Vec::new();
        v.write_var_len(0);
        v.write_var_len(0x40);
        v.write_var_len(0x7f);
        v.write_var_len(0x80);
        v.write_var_len(0x2000);
        v.write_var_len(0x3fff);
        v.write_var_len(0x4000);
        v.write_var_len(0x100000);
        v.write_var_len(0x1fffff);
        v.write_var_len(0x200000);
        v.write_var_len(0x8000000);
        v.write_var_len(0xfffffff);
        bindiff(VAR_LEN, &v);
    }

    #[test]
    fn read_var_len() {
        let mut iter = VAR_LEN.iter();
        assert_eq!(iter.read_var_len(), 0);
        assert_eq!(iter.read_var_len(), 0x40);
        assert_eq!(iter.read_var_len(), 0x7f);
        assert_eq!(iter.read_var_len(), 0x80);
        assert_eq!(iter.read_var_len(), 0x2000);
        assert_eq!(iter.read_var_len(), 0x3fff);
        assert_eq!(iter.read_var_len(), 0x4000);
        assert_eq!(iter.read_var_len(), 0x100000);
        assert_eq!(iter.read_var_len(), 0x1fffff);
        assert_eq!(iter.read_var_len(), 0x200000);
        assert_eq!(iter.read_var_len(), 0x8000000);
        assert_eq!(iter.read_var_len(), 0xfffffff);
    }

    #[test]
    fn format0() {
        use Event::*;
        let mut trk = Track::new();
        trk.put_event(0, TimeSignature(4, 2, 24, 8)).unwrap();
        trk.put_event(0, SetTempo(500000)).unwrap();
        trk.put_event(0, ProgramChange(0, 5)).unwrap();
        trk.put_event(0, ProgramChange(1, 46)).unwrap();
        trk.put_event(0, ProgramChange(2, 70)).unwrap();
        trk.put_event(0, NoteOn(2, 48, 96)).unwrap();
        trk.put_event(0, NoteOn(2, 60, 96)).unwrap();
        trk.advance(96).unwrap();
        trk.put_event(0, NoteOn(1, 67, 64)).unwrap();
        trk.advance(96).unwrap();
        trk.put_event(0, NoteOn(0, 76, 32)).unwrap();
        trk.advance(192).unwrap();
        trk.put_event(0, NoteOff(2, 48, 64)).unwrap();
        trk.put_event(0, NoteOff(2, 60, 64)).unwrap();
        trk.put_event(0, NoteOff(1, 67, 64)).unwrap();
        trk.put_event(0, NoteOff(0, 76, 64)).unwrap();
        let header = Header::new(0, 1, 96).unwrap();
        let data = trk.finish().unwrap();
        let result = [&header.0, data].concat();
        bindiff(FILE0, result.as_slice());
    }

    #[test]
    fn format1() {
        use Event::*;
        let mut trk0 = Track::new();
        let mut trk1 = Track::new();
        let mut trk2 = Track::new();
        let mut trk3 = Track::new();
        trk0.put_event(0, TimeSignature(4, 2, 24, 8)).unwrap();
        trk0.put_event(0, SetTempo(500000)).unwrap();
        trk0.advance(384).unwrap();
        trk1.put_event(0, ProgramChange(0, 5)).unwrap();
        trk1.advance(192).unwrap();
        trk1.put_event(0, NoteOn(0, 76, 32)).unwrap();
        trk1.advance(192).unwrap();
        trk1.put_event(0, NoteOn(0, 76, 0)).unwrap();
        trk2.put_event(0, ProgramChange(1, 46)).unwrap();
        trk2.advance(96).unwrap();
        trk2.put_event(0, NoteOn(1, 67, 64)).unwrap();
        trk2.advance(288).unwrap();
        trk2.put_event(0, NoteOn(1, 67, 0)).unwrap();
        trk3.put_event(0, ProgramChange(2, 70)).unwrap();
        trk3.put_event(0, NoteOn(2, 48, 96)).unwrap();
        trk3.put_event(0, NoteOn(2, 60, 96)).unwrap();
        trk3.advance(384).unwrap();
        trk3.put_event(0, NoteOn(2, 48, 0)).unwrap();
        trk3.put_event(0, NoteOn(2, 60, 0)).unwrap();
        let header = Header::new(1, 4, 96).unwrap();
        let result = [
            &header.0,
            trk0.finish().unwrap(),
            trk1.finish().unwrap(),
            trk2.finish().unwrap(),
            trk3.finish().unwrap(),
        ]
        .concat();
        bindiff(FILE1, result.as_slice());
    }
}
