use core::task::local_data::*;
use std::dlist::DList;

struct Transaction<P> {
    state           : @mut PartitionState
}

pub struct PartitionState {
    depth           : int,
    processing_post : bool,
    postQ           : @mut DList<@fn()>
}

struct Partition<P> {
    state           : @mut PartitionState
    //key             : LocalDataKey<'static, @mut PartitionState>
}

pub impl<P> Partition<P> {
    pub fn new(k : LocalDataKey<'static, @mut PartitionState>) -> Partition<P> {
        Partition {
            state : @mut PartitionState {
                depth           : 0,
                processing_post : false,
                postQ           : DList()
            }
            //key   : k
        }
    }
}

pub struct DefPart;
pub fn def_part_key(_: @@mut PartitionState) {}

fn transactionally<P,A>(part : &Partition<P>, code : @fn(trans : &Transaction<P>) -> A) -> A {
    unsafe {
        let key = def_part_key;  // part.key;
        let ostate = local_data_get(key);
        match ostate {
            Some(state) => { code(&Transaction { state : *state }) }
            None        => {
                let trans = @Transaction {
                    state : part.state
                };
                local_data_set(key, @trans.state);
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
