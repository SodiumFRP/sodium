#!/bin/sh -e
rustc sodium/sodium.rc
rustc test_sodium.rs -Lsodium
