#![allow(dead_code)]

mod env;
mod error;
mod include;
mod interpreter;
mod lexer;
mod midi;
mod parser;
mod value;

#[cfg(target_arch = "wasm32")]
pub mod wasm;

use error::Result;
use include::DEFAULT;
use interpreter::Interpreter;
use std::io::Write;
use value::Value;

pub fn run<W: Write>(writer: &mut W, src: &str) -> Result<Value> {
    let src1 = [DEFAULT, src].concat();
    let mut itp = Interpreter::new();
    itp.run(writer, &src1)
}

#[cfg(test)]
mod tests {
    use super::midi::tests::{bindiff, FILE0, FILE1};
    use super::run;
    use std::io::Cursor;

    #[test]
    fn fmt0_nolib() {
        let src = "(put-event 0 0 (time-signature 4 2 24 8))
                   (put-event 0 0 (set-tempo 500000))
                   (put-event 0 0 (program-change 0 5))
                   (put-event 0 0 (program-change 1 46))
                   (put-event 0 0 (program-change 2 70))
                   (put-event 0 0 (note-on 2 48 96))
                   (put-event 0 0 (note-on 2 60 96))
                   (put-event 0 384 (note-off 2 48 64))
                   (put-event 0 384 (note-off 2 60 64))
                   (adv-clock 0 96)
                   (put-event 0 0 (note-on 1 67 64))
                   (put-event 0 288 (note-off 1 67 64))
                   (adv-clock 0 96)
                   (put-event 0 0 (note-on 0 76 32))
                   (put-event 0 192 (note-off 0 76 64))
                   (adv-clock 0 192)";
        let mut w = Cursor::new(Vec::new());
        run(&mut w, src).unwrap();
        bindiff(FILE0, &w.into_inner());
    }

    #[test]
    fn fmt1_nolib() {
        let src = "(define fmt 1)
                   (put-event 0 0 (time-signature 4 2 24 8))
                   (put-event 0 0 (set-tempo 500000))
                   (adv-clock 0 384)
                   (put-event 1 0 (program-change 0 5))
                   (adv-clock 1 192)
                   (put-event 1 0 (note-on 0 76 32))
                   (put-event 1 192 (note-on 0 76 0))
                   (adv-clock 1 192)
                   (put-event 2 0 (program-change 1 46))
                   (adv-clock 2 96)
                   (put-event 2 0 (note-on 1 67 64))
                   (put-event 2 288 (note-on 1 67 0))
                   (adv-clock 2 288)
                   (put-event 3 0 (program-change 2 70))
                   (put-event 3 0 (note-on 2 48 96))
                   (put-event 3 384 (note-on 2 48 0))
                   (put-event 3 0 (note-on 2 60 96))
                   (put-event 3 384 (note-on 2 60 0))
                   (adv-clock 3 384)";
        let mut w = Cursor::new(Vec::new());
        run(&mut w, src).unwrap();
        bindiff(FILE1, &w.into_inner());
    }

    #[test]
    fn fmt0_lib() {
        let src = "(time 4 2 24 8)
                   (tempo 120)
                   (program 5)
                   (set chn 1)
                   (program 46)
                   (set chn 2)
                   (program 70)
                   (on 2 48 96)
                   (on 2 60 96)
                   (rest 4)
                   (on 1 67 64)
                   (rest 4)
                   (on 0 76 32)
                   (rest 2)
                   (off 2 48 64)
                   (off 2 60 64)
                   (off 1 67 64)
                   (off 0 76 64)";
        let mut w = Cursor::new(Vec::new());
        run(&mut w, src).unwrap();
        bindiff(FILE0, &w.into_inner());
    }

    #[test]
    fn fmt1_lib() {
        let src = "(set fmt 1)
                   (time 4 2 24 8)
                   (tempo 120)
                   (rest 1)
                   (set trk 1) 
                   (set chn 0)
                   (program 5)
                   (rest 2)
                   (note 76 2 2 32)
                   (set trk 2) 
                   (set chn 1)
                   (program 46)
                   (rest 4)
                   (on 1 67 64)
                   (3 (rest 4))
                   (on 1 67 0)
                   (set trk 3) 
                   (set chn 2)
                   (program 70)
                   (on 2 48 96)
                   (on 2 60 96)
                   (rest 1)
                   (on 2 48 0)
                   (on 2 60 0)";
        let mut w = Cursor::new(Vec::new());
        run(&mut w, src).unwrap();
        bindiff(FILE1, &w.into_inner());
    }

    #[test]
    fn nothing() {
        let mut w = Cursor::new(Vec::new());
        run(&mut w, "").unwrap();
        bindiff(&[], &w.into_inner());
    }
}
