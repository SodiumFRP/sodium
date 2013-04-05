use core::task::local_data::*;
use std::dlist::DList;

struct Transaction<P> {
    part            : @mut PartitionState<P>
}

pub struct PartitionState<P> {
    depth           : int,
    processing_post : bool,
    postQ           : @mut DList<@fn()>
}

struct Partition<P> {
    state           : @mut PartitionState<P>,
    key             : LocalDataKey<'static, Transaction<P>>
}

pub impl<P> Partition<P> {
    pub fn new(k : LocalDataKey<'static, Transaction<P>>) -> Partition<P> {
        Partition {
            state : @mut PartitionState {
                depth           : 0,
                processing_post : false,
                postQ           : DList()
            },
            key   : k
        }
    }
}

pub struct DefPart;
pub fn def_part_key(_: @Transaction<DefPart>) {}

fn transactionally<P,A>(part : &Partition<P>, code : @fn(trans : &Transaction<P>) -> A) -> A {
    unsafe {
        let key = part.key;
        let otrans = local_data_get(key);
        match otrans {
            Some(trans) => { code(&*trans) }
            None        => {
                let trans = @Transaction {
                    part : part.state
                };
                local_data_set(key, trans);
                let ret = code(&*trans);
                local_data_pop(key);
                ret
            }
        }
    }
}

pub fn sync<P,A>(part : &Partition<P>, code : @fn() -> A) -> A {
    transactionally(part, |_| { code() })
}
