use std::list::*;
use core::cmp::Eq;
use transaction::*;

type Listen<A> = @fn(@fn(A)) -> @fn();

struct Event<P,A> {
    part: Partition<P>,
    listener: Listen<A>
}

pub impl<P,A : Copy> Event<P,A> {
    pub fn listen(&self, handler : @fn(A)) -> @fn() {
        return (self.listener)(handler);
    }

    pub fn new(part : Partition<P>) -> (Event<P,A>, @fn(A)) {
        let state = @mut EventState{ handlers : @Nil, nextIx : 0 };
        (
            Event {
                part : part,
                listener : |handler| {
                        let ix = state.nextIx;
                        state.handlers = @Cons((ix, handler), state.handlers);
                        state.nextIx = state.nextIx + 1;
                        || { state.handlers = delete(state.handlers, &ix) } 
                    }
            },
            |a| {
                for each(state.handlers) |&(_,h)| { h(a) };
            }
        )
    }
}

fn delete<A: Eq + Copy,B: Copy>(l : &List<(A,B)>, key : &A) -> @List<(A,B)>
{
    match l {
        &Nil             => @Nil,
        &Cons((k, v), t) => {
            if k == *key { delete(t, key) }
                    else { @Cons((k, v), delete(t, key)) }
        }
    }
}

struct EventState<A> {
    handlers : @List<(int, @fn(A))>,
    nextIx   : int
}

