use std::env;
use std::ffi::OsString;
use std::fs;
use std::io;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::process;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

const USAGE: &str = "\nUsage: midilisp [INPUT] [OUTPUT]";

fn main() {
    process::exit(match run() {
        Ok(()) => 0,
        Err(err) => {
            eprint!("{}", err);
            1
        }
    })
}

fn run() -> Result<()> {
    let opts = parse_args()?;
    let src = read_input(&opts)?;
    let mut writer = create_writer(&opts)?;
    let log = midilisp::run(&mut writer, &src)?;
    finish(&opts, log)
}

struct Opts {
    in_path: Option<PathBuf>,
    out_path: Option<PathBuf>,
    tmp_path: Option<PathBuf>,
}

impl Opts {
    fn new<P: Into<PathBuf>>(i: Option<P>, o: Option<P>) -> Self {
        let (op, tp) = match o {
            Some(p) => {
                let op: PathBuf = p.into();
                let mut tp = op.clone();
                tp.set_extension("tmp");
                (Some(op), Some(tp))
            }
            None => (None, None),
        };

        Self {
            in_path: i.map(|p| p.into()),
            out_path: op,
            tmp_path: tp,
        }
    }
}

fn parse_args() -> Result<Opts> {
    let args: Vec<OsString> = env::args_os().collect();

    match args.len() {
        1 | 2 | 3 => Ok(Opts::new(args.get(1), args.get(2))),
        _ => Err(CliError.into()),
    }
}

fn read_input(opts: &Opts) -> Result<String> {
    match &opts.in_path {
        Some(path) => Ok(fs::read_to_string(path)?),
        None => {
            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf)?;
            Ok(buf)
        }
    }
}

fn create_writer(opts: &Opts) -> Result<Box<dyn Write>> {
    match &opts.tmp_path {
        Some(path) => Ok(Box::new(fs::File::create(path)?)),
        None => Ok(Box::new(io::stdout())),
    }
}

fn finish(opts: &Opts, log: String) -> Result<()> {
    match &opts.out_path {
        Some(out) => {
            let tmp = opts.tmp_path.as_ref().unwrap();
            let len = tmp.metadata()?.len();

            if len > 0 {
                fs::rename(&tmp, &out)?;
                eprintln!("{} bytes written to {}", len, &out.to_string_lossy());
            } else {
                eprint!("{}", log);
                fs::remove_file(&tmp)?;
            }
        }
        None => {}
    }

    Ok(())
}

#[derive(thiserror::Error, Copy, Clone, Debug)]
#[error("invalid number of arguments{}", USAGE)]
struct CliError;
