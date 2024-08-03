use std::fmt::{Display, Formatter};
use std::process::{Command, Output};
use std::str::Utf8Error;
use std::{env, error, io};

const NIGHTLY_CHANNELS: &[&str] = &["nightly", "dev"];

fn main() {
    if NIGHTLY_CHANNELS.contains(&channel().unwrap().as_str()) {
        println!("cargo:rustc-cfg=nightly_rustc");
    }
}

// A homemade parser of the `rustc -vV` output, because we don't
// have access to the `rustc_version` or `semver` crate.
fn channel() -> Result<String, Error> {
    let cmd = env::var_os("RUSTC").unwrap_or("rustc".into());
    let mut cmd = Command::new(cmd);
    let out = cmd.arg("-vV").output().map_err(Error::ExecutionFailed)?;
    out.status
        .success()
        .then_some(())
        .ok_or(Error::for_command(&out))?;
    let out = std::str::from_utf8(&out.stdout).map_err(Error::InvalidOutput)?;

    let release = out
        .lines()
        .find(|&line| line.starts_with("release:"))
        .ok_or(Error::for_missing_release(out))?;
    release
        .split('-')
        .last()
        .map(Into::into)
        .ok_or(Error::InvalidReleaseTag(release.into()))
}

#[derive(Debug)]
enum Error {
    ExecutionFailed(io::Error),
    CommandFailed { stdout: String, stderr: String },
    InvalidOutput(Utf8Error),
    ReleaseNotFound { output: String },
    InvalidReleaseTag(String),
}

impl Error {
    fn for_command(output: &Output) -> Self {
        Self::CommandFailed {
            stdout: String::from_utf8_lossy(&output.stdout).into(),
            stderr: String::from_utf8_lossy(&output.stderr).into(),
        }
    }

    fn for_missing_release(out: &str) -> Self {
        Self::ReleaseNotFound { output: out.into() }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExecutionFailed(io_error) => {
                write!(f, "execution of command failed -- error:\n\n{io_error}")
            },
            Self::CommandFailed { stdout, stderr } => {
                write!(f, "error running command -- stdout:\n\n{stdout}\n\nstderr:\n\n{stderr}")
            },
            Self::InvalidOutput(utf8_error) => {
                write!(f, "error parsing output -- error:\n\n{utf8_error}")
            },
            Self::ReleaseNotFound { output } => {
                write!(f, "release tag not found in rustc output -- output:\n\n{output}")
            },
            Self::InvalidReleaseTag(tag) => write!(f, "invalid release tag -- {tag}"),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::ExecutionFailed(io_error) => Some(io_error),
            Self::InvalidOutput(utf8_error) => Some(utf8_error),
            _ => None,
        }
    }
}
