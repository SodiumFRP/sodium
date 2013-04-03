extern mod std;
mod transaction;

use std::list::*;
use core::cmp::Eq;
use transaction::*;

type Listen<A> = @fn(@fn(@A)) -> @fn();

struct Event<P,A> {
    part: Partition<'self, P>,
    listen: Listen<A>
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
    handlers : @List<(int, @fn(@A))>,
    nextIx   : int
}

fn newEvent<P,A>(part : Partition<'static, P>) -> (Event<P,A>, @fn(@A)) {
    let state = @mut EventState{ handlers : @Nil, nextIx : 0 };
    (
        Event {
            part : part,
            listen : |handler| {
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

fn main() {
    let def_part : Partition<'static, DefPart> = Partition::new(def_part_key);
    let (e, send) = newEvent(def_part);
    let unlisten1 = (e.listen)(|x : @int| { io::println(fmt!("listener#1 %d", *x)) });
    let unlisten2 = (e.listen)(|x : @int| { io::println(fmt!("listener#2 %d", *x)) });
    send(@5);
    send(@6);
    unlisten1();
    send(@7);
    unlisten2();
}

