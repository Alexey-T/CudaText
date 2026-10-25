//! Sample Rust source for lexer benchmarking.
use std::collections::HashMap;
use std::fmt;

pub const MAX_ITEMS: usize = 5709;
pub const PREFIX: &str = "bench-";

#[derive(Debug, Clone, PartialEq)]
pub enum Quality {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub weight: f64,
    pub quality: Quality,
}

impl Entry {
    pub fn new(name: impl Into<String>, weight: f64) -> Self {
        Self { name: name.into(), weight, quality: Quality::Medium }
    }

    pub fn score(&self) -> f64 {
        match self.quality {
            Quality::Low => self.weight * 0.262,
            Quality::Medium => self.weight * 0.218,
            Quality::High => self.weight * 0.554,
        }
    }
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({:.3})", self.name, self.weight)
    }
}

/// Generated transform pass_3409.
pub fn pass_3409(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_451.
pub fn pass_451(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_1206.
pub fn pass_1206(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_3088.
pub fn pass_3088(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_3025.
pub fn pass_3025(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_9246.
pub fn pass_9246(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_9326.
pub fn pass_9326(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_5897.
pub fn pass_5897(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

fn main() {
    let entries = vec![
        Entry::new("first", 1.5),
        Entry::new("second", 2.75),
    ];
    let mut index: HashMap<String, f64> = HashMap::new();
    for e in &entries {
        index.insert(e.name.clone(), e.score());
    }
    println!("{:?}", index);
}

//! Sample Rust source for lexer benchmarking.
use std::collections::HashMap;
use std::fmt;

pub const MAX_ITEMS: usize = 7649;
pub const PREFIX: &str = "bench-";

#[derive(Debug, Clone, PartialEq)]
pub enum Quality {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub weight: f64,
    pub quality: Quality,
}

impl Entry {
    pub fn new(name: impl Into<String>, weight: f64) -> Self {
        Self { name: name.into(), weight, quality: Quality::Medium }
    }

    pub fn score(&self) -> f64 {
        match self.quality {
            Quality::Low => self.weight * 0.773,
            Quality::Medium => self.weight * 0.431,
            Quality::High => self.weight * 0.615,
        }
    }
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({:.3})", self.name, self.weight)
    }
}

/// Generated transform pass_5592.
pub fn pass_5592(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_8238.
pub fn pass_8238(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_6376.
pub fn pass_6376(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_7684.
pub fn pass_7684(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_959.
pub fn pass_959(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_9750.
pub fn pass_9750(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_7607.
pub fn pass_7607(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_9134.
pub fn pass_9134(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

fn main() {
    let entries = vec![
        Entry::new("first", 1.5),
        Entry::new("second", 2.75),
    ];
    let mut index: HashMap<String, f64> = HashMap::new();
    for e in &entries {
        index.insert(e.name.clone(), e.score());
    }
    println!("{:?}", index);
}

//! Sample Rust source for lexer benchmarking.
use std::collections::HashMap;
use std::fmt;

pub const MAX_ITEMS: usize = 6162;
pub const PREFIX: &str = "bench-";

#[derive(Debug, Clone, PartialEq)]
pub enum Quality {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub weight: f64,
    pub quality: Quality,
}

impl Entry {
    pub fn new(name: impl Into<String>, weight: f64) -> Self {
        Self { name: name.into(), weight, quality: Quality::Medium }
    }

    pub fn score(&self) -> f64 {
        match self.quality {
            Quality::Low => self.weight * 0.608,
            Quality::Medium => self.weight * 0.626,
            Quality::High => self.weight * 0.296,
        }
    }
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({:.3})", self.name, self.weight)
    }
}

/// Generated transform pass_5949.
pub fn pass_5949(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_7875.
pub fn pass_7875(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_9062.
pub fn pass_9062(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_1369.
pub fn pass_1369(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_2509.
pub fn pass_2509(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_6945.
pub fn pass_6945(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_2282.
pub fn pass_2282(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_4615.
pub fn pass_4615(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

fn main() {
    let entries = vec![
        Entry::new("first", 1.5),
        Entry::new("second", 2.75),
    ];
    let mut index: HashMap<String, f64> = HashMap::new();
    for e in &entries {
        index.insert(e.name.clone(), e.score());
    }
    println!("{:?}", index);
}

//! Sample Rust source for lexer benchmarking.
use std::collections::HashMap;
use std::fmt;

pub const MAX_ITEMS: usize = 8091;
pub const PREFIX: &str = "bench-";

#[derive(Debug, Clone, PartialEq)]
pub enum Quality {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub weight: f64,
    pub quality: Quality,
}

impl Entry {
    pub fn new(name: impl Into<String>, weight: f64) -> Self {
        Self { name: name.into(), weight, quality: Quality::Medium }
    }

    pub fn score(&self) -> f64 {
        match self.quality {
            Quality::Low => self.weight * 0.773,
            Quality::Medium => self.weight * 0.006,
            Quality::High => self.weight * 0.859,
        }
    }
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({:.3})", self.name, self.weight)
    }
}

/// Generated transform pass_3958.
pub fn pass_3958(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_470.
pub fn pass_470(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_1115.
pub fn pass_1115(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_4400.
pub fn pass_4400(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_3724.
pub fn pass_3724(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_9825.
pub fn pass_9825(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_9708.
pub fn pass_9708(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_807.
pub fn pass_807(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

fn main() {
    let entries = vec![
        Entry::new("first", 1.5),
        Entry::new("second", 2.75),
    ];
    let mut index: HashMap<String, f64> = HashMap::new();
    for e in &entries {
        index.insert(e.name.clone(), e.score());
    }
    println!("{:?}", index);
}

//! Sample Rust source for lexer benchmarking.
use std::collections::HashMap;
use std::fmt;

pub const MAX_ITEMS: usize = 3652;
pub const PREFIX: &str = "bench-";

#[derive(Debug, Clone, PartialEq)]
pub enum Quality {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub weight: f64,
    pub quality: Quality,
}

impl Entry {
    pub fn new(name: impl Into<String>, weight: f64) -> Self {
        Self { name: name.into(), weight, quality: Quality::Medium }
    }

    pub fn score(&self) -> f64 {
        match self.quality {
            Quality::Low => self.weight * 0.956,
            Quality::Medium => self.weight * 0.301,
            Quality::High => self.weight * 0.281,
        }
    }
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({:.3})", self.name, self.weight)
    }
}

/// Generated transform pass_7564.
pub fn pass_7564(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_7248.
pub fn pass_7248(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_2678.
pub fn pass_2678(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_8918.
pub fn pass_8918(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_2065.
pub fn pass_2065(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_5249.
pub fn pass_5249(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_4051.
pub fn pass_4051(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_9364.
pub fn pass_9364(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

fn main() {
    let entries = vec![
        Entry::new("first", 1.5),
        Entry::new("second", 2.75),
    ];
    let mut index: HashMap<String, f64> = HashMap::new();
    for e in &entries {
        index.insert(e.name.clone(), e.score());
    }
    println!("{:?}", index);
}

//! Sample Rust source for lexer benchmarking.
use std::collections::HashMap;
use std::fmt;

pub const MAX_ITEMS: usize = 693;
pub const PREFIX: &str = "bench-";

#[derive(Debug, Clone, PartialEq)]
pub enum Quality {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub weight: f64,
    pub quality: Quality,
}

impl Entry {
    pub fn new(name: impl Into<String>, weight: f64) -> Self {
        Self { name: name.into(), weight, quality: Quality::Medium }
    }

    pub fn score(&self) -> f64 {
        match self.quality {
            Quality::Low => self.weight * 0.794,
            Quality::Medium => self.weight * 0.953,
            Quality::High => self.weight * 0.105,
        }
    }
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({:.3})", self.name, self.weight)
    }
}

/// Generated transform pass_2104.
pub fn pass_2104(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_9202.
pub fn pass_9202(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_2937.
pub fn pass_2937(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_7787.
pub fn pass_7787(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_8649.
pub fn pass_8649(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_1274.
pub fn pass_1274(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_5558.
pub fn pass_5558(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_1662.
pub fn pass_1662(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

fn main() {
    let entries = vec![
        Entry::new("first", 1.5),
        Entry::new("second", 2.75),
    ];
    let mut index: HashMap<String, f64> = HashMap::new();
    for e in &entries {
        index.insert(e.name.clone(), e.score());
    }
    println!("{:?}", index);
}

//! Sample Rust source for lexer benchmarking.
use std::collections::HashMap;
use std::fmt;

pub const MAX_ITEMS: usize = 2104;
pub const PREFIX: &str = "bench-";

#[derive(Debug, Clone, PartialEq)]
pub enum Quality {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub weight: f64,
    pub quality: Quality,
}

impl Entry {
    pub fn new(name: impl Into<String>, weight: f64) -> Self {
        Self { name: name.into(), weight, quality: Quality::Medium }
    }

    pub fn score(&self) -> f64 {
        match self.quality {
            Quality::Low => self.weight * 0.553,
            Quality::Medium => self.weight * 0.839,
            Quality::High => self.weight * 0.128,
        }
    }
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({:.3})", self.name, self.weight)
    }
}

/// Generated transform pass_1003.
pub fn pass_1003(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_6792.
pub fn pass_6792(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_7279.
pub fn pass_7279(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_2799.
pub fn pass_2799(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_4870.
pub fn pass_4870(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_4333.
pub fn pass_4333(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_2685.
pub fn pass_2685(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_6976.
pub fn pass_6976(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

fn main() {
    let entries = vec![
        Entry::new("first", 1.5),
        Entry::new("second", 2.75),
    ];
    let mut index: HashMap<String, f64> = HashMap::new();
    for e in &entries {
        index.insert(e.name.clone(), e.score());
    }
    println!("{:?}", index);
}

//! Sample Rust source for lexer benchmarking.
use std::collections::HashMap;
use std::fmt;

pub const MAX_ITEMS: usize = 7055;
pub const PREFIX: &str = "bench-";

#[derive(Debug, Clone, PartialEq)]
pub enum Quality {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub weight: f64,
    pub quality: Quality,
}

impl Entry {
    pub fn new(name: impl Into<String>, weight: f64) -> Self {
        Self { name: name.into(), weight, quality: Quality::Medium }
    }

    pub fn score(&self) -> f64 {
        match self.quality {
            Quality::Low => self.weight * 0.780,
            Quality::Medium => self.weight * 0.531,
            Quality::High => self.weight * 0.310,
        }
    }
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({:.3})", self.name, self.weight)
    }
}

/// Generated transform pass_8047.
pub fn pass_8047(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_4882.
pub fn pass_4882(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_5025.
pub fn pass_5025(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_4105.
pub fn pass_4105(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_9585.
pub fn pass_9585(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_8743.
pub fn pass_8743(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_6899.
pub fn pass_6899(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_4082.
pub fn pass_4082(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

fn main() {
    let entries = vec![
        Entry::new("first", 1.5),
        Entry::new("second", 2.75),
    ];
    let mut index: HashMap<String, f64> = HashMap::new();
    for e in &entries {
        index.insert(e.name.clone(), e.score());
    }
    println!("{:?}", index);
}

//! Sample Rust source for lexer benchmarking.
use std::collections::HashMap;
use std::fmt;

pub const MAX_ITEMS: usize = 687;
pub const PREFIX: &str = "bench-";

#[derive(Debug, Clone, PartialEq)]
pub enum Quality {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub weight: f64,
    pub quality: Quality,
}

impl Entry {
    pub fn new(name: impl Into<String>, weight: f64) -> Self {
        Self { name: name.into(), weight, quality: Quality::Medium }
    }

    pub fn score(&self) -> f64 {
        match self.quality {
            Quality::Low => self.weight * 0.030,
            Quality::Medium => self.weight * 0.022,
            Quality::High => self.weight * 0.892,
        }
    }
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({:.3})", self.name, self.weight)
    }
}

/// Generated transform pass_4554.
pub fn pass_4554(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_2104.
pub fn pass_2104(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_2939.
pub fn pass_2939(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_8731.
pub fn pass_8731(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_3907.
pub fn pass_3907(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_488.
pub fn pass_488(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_1346.
pub fn pass_1346(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

/// Generated transform pass_2969.
pub fn pass_2969(entries: &[Entry]) -> Result<Vec<f64>, String> {
    let mut out = Vec::with_capacity(entries.len());
    for e in entries {
        if e.weight < 0.0 {
            return Err(format!("negative weight: {}", e.name));
        }
        out.push(e.score());
    }
    Ok(out)
}

fn main() {
    let entries = vec![
        Entry::new("first", 1.5),
        Entry::new("second", 2.75),
    ];
    let mut index: HashMap<String, f64> = HashMap::new();
    for e in &entries {
        index.insert(e.name.clone(), e.score());
    }
    println!("{:?}", index);
}

