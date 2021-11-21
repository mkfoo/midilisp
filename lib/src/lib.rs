#![allow(dead_code)]

mod env;
mod error;
mod interpreter;
mod lexer;
mod midi;
mod parser;
mod value;

#[cfg(target_arch = "wasm32")]
pub mod wasm;

use error::WithContext;
use fnv::FnvBuildHasher;
use indexmap::{IndexMap, IndexSet};
use interpreter::Interpreter;
use std::io::Write;

pub(crate) type FnvIndexMap<K, V> = IndexMap<K, V, FnvBuildHasher>;
pub(crate) type FnvIndexSet<T> = IndexSet<T, FnvBuildHasher>;

pub fn run<W: Write>(writer: &mut W, src: &str) -> std::result::Result<String, WithContext> {
    let mut itp = Interpreter::new();

    match itp.run(writer, src) {
        Ok(val) => {
            let log = itp.get_log();

            if log.is_empty() {
                Ok(format!("{}\n", val))
            } else {
                Ok(log)
            }
        }
        Err(e) => Err(itp.get_context(e)),
    }
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
        let src = "(include \"default\")
                   (time 4 2 24 8)
                   (tempo 120)
                   (set chn 0)
                   (program 6)
                   (on 76 192 32)
                   (set chn 1)
                   (program 47)
                   (on 67 96 64)
                   (set chn 2)
                   (program 71)
                   (on 48 0 96)
                   (on 60 0 96)
                   (rest 1)
                   (off 48 0 64)
                   (off 60 0 64)
                   (set chn 1)
                   (off 67 0 64)
                   (set chn 0)
                   (off 76 0 64)";
        let mut w = Cursor::new(Vec::new());
        run(&mut w, src).unwrap();
        bindiff(FILE0, &w.into_inner());
    }

    #[test]
    fn fmt1_lib() {
        let src = "(include \"default\")
                   (set fmt 1)
                   (time 4 2 24 8)
                   (tempo 120)
                   (rest 1)
                   (set trk 1) 
                   (set chn 0)
                   (program 6)
                   (rest 2)
                   (note 76 2 2 32)
                   (set trk 2) 
                   (set chn 1)
                   (program 47)
                   (rest 4)
                   (note 67 1.33 1.33 64)
                   (set trk 3) 
                   (set chn 2)
                   (program 71)
                   (note 48 0 1 96)
                   (note 60 0 1 96)";
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
