use core::task::local_data::*;
use std::dlist::DList;

struct Transaction<P>;

struct PartitionState {
    depth           : int,
    processing_post : bool,
    postQ           : @mut DList<@fn()>
}

struct Partition<P> {
    state : @mut PartitionState,
    key   : LocalDataKey<'static, Transaction<P>>
}

pub impl<P> Partition<P> {
    pub fn new(k : LocalDataKey<'static, Transaction<P>>) -> Partition<P> {
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

pub struct DefPart;
pub fn def_part_key(_: @Transaction<DefPart>) {}

