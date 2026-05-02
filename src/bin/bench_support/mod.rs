#![allow(dead_code)]

use anyhow::{Context, Result, bail};

pub mod allocation;
pub mod cold;
pub mod render;

pub const SYNTHETIC_SEARCH_QUERIES: &[&str] = &[
    "b",
    "be",
    "ben",
    "bench",
    "benchmark",
    "target",
    "src",
    "rs",
    "guide",
    "workspace",
    "relative",
    "omega",
    "tmp",
    "zzz",
];

pub const SYNTHETIC_QUERY_STEPS: &[&str] = &[
    "b", "be", "ben", "be", "ben", "benc", "bench", "ben", "be", "", "s", "sr", "src", "sr", "s",
    "", "t", "ta", "tar", "targ", "target", "targe", "target", "", "o", "om", "ome", "omeg",
    "omega", "ome", "omega", "", "z", "zz", "zzz", "zz", "",
];

pub const REALISTIC_SEARCH_QUERIES: &[&str] = &[
    "s",
    "src",
    "search.rs",
    "cargo",
    "error",
    "target",
    "git",
    "/home",
    "release",
    "warning",
    "https",
    "flash",
    "é",
    "zzzz",
];

pub const REALISTIC_QUERY_STEPS: &[&str] = &[
    "c",
    "ca",
    "car",
    "carg",
    "cargo",
    "carg",
    "cargo",
    "",
    "s",
    "sr",
    "src",
    "src/",
    "src/s",
    "src/se",
    "src/sea",
    "src/sear",
    "src/search",
    "src/search.",
    "src/search.rs",
    "src/search.",
    "src/search.rs",
    "",
    "t",
    "ta",
    "tar",
    "targ",
    "targe",
    "target",
    "taret",
    "target",
    "",
    "e",
    "er",
    "err",
    "erro",
    "error",
    "err",
    "error",
    "",
    "g",
    "gi",
    "git",
    "",
    "h",
    "ht",
    "htt",
    "http",
    "https",
    "",
    "z",
    "zz",
    "zzz",
    "zzzz",
    "zzz",
    "",
];

pub const COLD_SELECTION_PREFIXES: &[&str] = &["car", "src", "tar", "git", "fla"];

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Scenario {
    Synthetic,
    ShellSparse,
    Development,
    CopyMode,
}

impl Scenario {
    pub fn parse(value: Option<&str>, program: &str) -> Result<Self> {
        match value {
            None | Some("synthetic") => Ok(Self::Synthetic),
            Some("shell-sparse") => Ok(Self::ShellSparse),
            Some("development") => Ok(Self::Development),
            Some("copy-mode") => Ok(Self::CopyMode),
            Some(value) => bail!(
                "unknown scenario {value:?}; usage: {program} <input-path> <iterations> [synthetic|shell-sparse|development|copy-mode]"
            ),
        }
    }

    pub fn is_realistic(self) -> bool {
        self != Self::Synthetic
    }

    pub fn search_queries(self) -> &'static [&'static str] {
        if self.is_realistic() {
            REALISTIC_SEARCH_QUERIES
        } else {
            SYNTHETIC_SEARCH_QUERIES
        }
    }

    pub fn query_steps(self) -> &'static [&'static str] {
        if self.is_realistic() {
            REALISTIC_QUERY_STEPS
        } else {
            SYNTHETIC_QUERY_STEPS
        }
    }

    pub fn visible_lines(self) -> usize {
        match self {
            Self::Synthetic => 20,
            Self::ShellSparse | Self::Development | Self::CopyMode => 51,
        }
    }

    pub fn captured_line_capacity(self) -> usize {
        match self {
            Self::Synthetic => 0,
            Self::ShellSparse | Self::Development | Self::CopyMode => self.visible_lines() + 2,
        }
    }
}

pub fn parse_args(program: &str) -> Result<(String, u64, Scenario)> {
    let mut args = std::env::args().skip(1);
    let Some(input_path) = args.next() else {
        bail!(
            "usage: {program} <input-path> <iterations> [synthetic|shell-sparse|development|copy-mode]"
        );
    };
    let Some(iterations) = args.next() else {
        bail!(
            "usage: {program} <input-path> <iterations> [synthetic|shell-sparse|development|copy-mode]"
        );
    };
    let scenario = Scenario::parse(args.next().as_deref(), program)?;
    if args.next().is_some() {
        bail!(
            "usage: {program} <input-path> <iterations> [synthetic|shell-sparse|development|copy-mode]"
        );
    }

    let iterations = iterations
        .parse::<u64>()
        .with_context(|| format!("invalid iterations: {iterations}"))?;
    if iterations == 0 {
        bail!("invalid iterations: {iterations}");
    }

    Ok((input_path, iterations, scenario))
}
