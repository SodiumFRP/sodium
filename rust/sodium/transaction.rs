use core::task::local_data::*;
use std::dlist::DList;

struct Transaction;

struct PartitionState {
    depth           : int,
    processing_post : bool,
    postQ           : @mut DList<@fn()>
}

pub struct Partition<P> {
    state : @mut PartitionState,
    key   : LocalDataKey<'static, Transaction>
}

pub impl<P> Partition<P> {
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

