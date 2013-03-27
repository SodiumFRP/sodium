#[link(name = "transaction", vers = "0.1", author = "Stephen Blackheath")];

// Make a library ("bin" is the default)
#[crate_type = "lib"];

// Turn on a warning
#[warn(non_camel_case_types)]

use task::local_data::*;
use core::dlist::*;

struct Transaction {
}

fn currentTransaction(_: @Transaction) {}

const key : LocalDataKey<Transaction> = currentTransaction;

struct Partition {
    depth           : int,
    processing_post : bool,
    postQ           : DList<fn()>,
}

