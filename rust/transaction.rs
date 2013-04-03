#[link(name = "transaction", vers = "0.1", author = "Stephen Blackheath")];

// Make a library ("bin" is the default)
#[crate_type = "lib"];

// Turn on a warning
#[warn(non_camel_case_types)]

extern mod std;
use core::task::local_data::*;
use std::dlist::DList;

struct Transaction;

fn currentTransaction(_: @Transaction) {}

//static key : LocalDataKey<'self, Transaction> = currentTransaction;

struct PartitionState {
    depth           : int,
    processing_post : bool,
    postQ           : @mut DList<@fn()>
}

pub struct Partition<P> {
    state : @mut PartitionState,
    key   : LocalDataKey<'static, Transaction>
}

impl<P> Partition<P> {
    pub fn new(k : LocalDataKey<'static, Transaction>) -> Partition<P> {
        Partition {
            state : @mut PartitionState {
                depth : 0,
                processing_post : false,
                postQ : DList()
            },
            key : k
        }
    }
}

pub fn def_part_key(_: @Transaction) {}
pub struct DefPart;

