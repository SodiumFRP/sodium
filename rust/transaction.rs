#[link(name = "transaction", vers = "0.1", author = "Stephen Blackheath")];

// Make a library ("bin" is the default)
#[crate_type = "lib"];

// Turn on a warning
#[warn(non_camel_case_types)]

use core::task::local_data::*;
use core::dlist::*;

struct Transaction {}

fn currentTransaction(_: @Transaction) {}

//static key : LocalDataKey<'self, Transaction> = currentTransaction;

struct PartitionState {
    depth           : int,
    processing_post : bool,
    postQ           : @mut DList<@fn()>
}

struct Partition<'self, P> {
    state : @mut PartitionState,
    key   : LocalDataKey<'self, Transaction>
}

impl<P> Partition<'self, P> {
    pub fn new(k : LocalDataKey<'self, Transaction>) -> Partition<'self, P> {
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
pub struct DefPart { }

