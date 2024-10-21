#![allow(unused)]
use std::{cmp::Ordering, fmt::Display};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct SpanPoint {
    row: u32,
    col: u32,
}

impl SpanPoint {
    pub fn new(row: u32, col: u32) -> Self {
        SpanPoint{ row, col }
    }

    pub fn shift_row(&mut self, len: i32) {
        self.col = 0;
        self.row = (self.row as i32 + len) as u32;
    }

    pub fn shift_col(&mut self, len: i32) {
        self.col = (self.col as i32 + len) as u32;
    }
}

impl PartialOrd for SpanPoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SpanPoint {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.row.cmp(&other.row) {
            Ordering::Less    => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            Ordering::Equal   => match self.col.cmp(&other.col) {
                Ordering::Less    => Ordering::Less,
                Ordering::Greater => Ordering::Greater,
                Ordering::Equal   => Ordering::Equal,
            }
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    begin: SpanPoint,
    end: SpanPoint,
}

impl Span {
    pub fn empty(point: SpanPoint) -> Self {
        Self{ begin: point, end: point }
    }

    pub fn single(begin: SpanPoint) -> Self {
        Self{ begin, end: SpanPoint{ row: begin.row, col: begin.col + 1 } }
    }

    pub fn new(begin: SpanPoint, end: SpanPoint) -> Self {
        Self{ begin, end }
    }


    pub fn union(left: Span, right: Span) -> Self {
        let begin = left.begin.min(right.begin);
        let end   = left.end.max(right.end);
        Span{ begin, end }
    }

    pub fn intersection(left: Span, right: Span) -> Self {
        let begin = left.begin.max(right.begin);
        let end   = left.end.min(right.end);
        Span{ begin, end }
    }

    pub fn shift_begin(&self, rhs: SpanPoint) -> Self {
        Span{ begin: rhs, end: self.end }
    }

    pub fn shift_end(&self, rhs: SpanPoint) -> Self {
        Span{ begin: self.begin, end: rhs }
    }


    pub fn shift_end_row(&mut self, len: i32) {
        self.end.shift_row(len);
    }

    pub fn shift_end_col(&mut self, len: i32) {
        self.end.shift_col(len);
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}:{})..({}:{})", self.begin.row + 1, self.begin.col + 1, self.end.row + 1, self.end.col + 1)
    }
}

