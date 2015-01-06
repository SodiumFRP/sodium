/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
// translated from bytestring cbits/fpstring.c
function h$fps_reverse(a_v, a_o, b_v, b_o, n) {
    if(n > 0) {
        var au8 = a_v.u8, bu8 = b_v.u8;
        for(var i=0;i<n;i++) {
            au8[a_o+n-i-1] = bu8[b_o+i];
        }
    }
}
function h$fps_intersperse(a_v,a_o,b_v,b_o,n,c) {
    if(n > 0) {
        var au8 = a_v.u8, bu8 = b_v.u8, dst_o = a_o;
        for(var i=0;i<n-1;i++) {
            au8[dst_o] = bu8[b_o+i];
            au8[dst_o+1] = c;
            dst_o += 2;
        }
        au8[dst_o] = bu8[b_o+n-1];
    }
}
function h$fps_maximum(a_v,a_o,n) {
    if(n > 0) {
        var au8 = a_v.u8, max = au8[a_o];
        for(var i=1;i<n;i++) {
            var c = au8[a_o+i];
            if(c > max) { max = c; }
        }
        return max;
    }
    return 0;
}
function h$fps_minimum(a_v,a_o,n) {
    if(n > 0) {
        var au8 = a_v.u8, min = a_v.u8[a_o];
        for(var i=1;i<n;i++) {
            var c = au8[a_o+i];
            if(c < min) { min = c; }
        }
        return min;
    }
    return 255;
}
function h$fps_count(a_v,a_o,n,c) {
    if(n > 0) {
        var au8 = a_v.u8, count = 0;
        for(var i=0;i<n;i++) {
            if(au8[a_o+i] === c) { count++; }
        }
        return count|0;
    }
    return 0;
}
function h$fps_memcpy_offsets(dst_d, dst_o, dst_off
                              , src_d, src_o, src_off, n) {
    return memcpy(dst_d, dst_o + dst_off, src_d, src_o + src_off, n);
}
// translated from bytestring cbits/itoa.c
var h$_hs_bytestring_digits = [48,49,50,51,52,53,54,55,56,57,97,98,99,100,101,102]; // 0123456789abcdef
var h$_hs_bytestring_l10 = goog.math.Long.fromBits(10, 0);
var h$_hs_bytestring_b10 = h$bigFromInt(10);
// signed integers
function h$_hs_bytestring_int_dec(x, buf_d, buf_o) {
    var c, ptr = buf_o, next_free, x_tmp;
    var bu8 = buf_d.u8;
    // we cannot negate directly as  0 - (minBound :: Int) = minBound
    if(x < 0) {
        bu8[ptr++] = 45; // '-'
        buf_o++;
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[ptr++] = h$_hs_bytestring_digits[x * 10 - x_tmp];
        if(x === 0) {
            h$ret1 = ptr;
            return buf_d;
        } else {
            x = -x;
        }
    }
    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[ptr++] = h$_hs_bytestring_digits[x_tmp - x * 10];
    } while (x);
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    h$ret1 = next_free;
    return buf_d;
}
// signed long long ints (64 bit integers)
function h$_hs_bytestring_long_long_int_dec(x_a, x_b, buf_d, buf_o) {
    var l10 = h$_hs_bytestring_l10;
    var x = goog.math.Long.fromBits(x_b, x_a);
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    // we cannot negate directly as  0 - (minBound :: Int) = minBound
    if(x.isNegative()) {
        bu8[ptr++] = 45; // '-';
        buf_o++;
        x_tmp = x;
        x = x.div(l10);
        bu8[ptr++] = h$_hs_bytestring_digits[x.multiply(l10).subtract(x_tmp).getLowBits()];
        if(x.isZero()) {
            h$ret1 = ptr;
            return buf_d;
        } else {
            x = x.negate();
        }
    }
    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x = x.div(l10);
        bu8[ptr++] = h$_hs_bytestring_digits[x_tmp.subtract(x.multiply(l10))];
    } while (!x.isZero());
    // reverse written digits
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    h$ret1 = next_free;
    return buf_d;
}
// unsigned integers
function h$_hs_bytestring_uint_dec(x, buf_d, buf_o) {
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    var x_tmp;
    if(x < 0) x += 4294967296;
    do {
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[ptr++] = h$_hs_bytestring_digits[x_tmp - x * 10];
    } while(x);
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    h$ret1 = next_free;
    return buf_d;
}
function h$_hs_bytestring_long_long_uint_dec(x_a, x_b, buf_d, buf_o) {
    var b10 = h$_hs_bytestring_b10;
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    var x = h$bigFromWord64(x_a, x_b), x_tmp;
    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x = x.divide(b10);
        bu8[ptr++] = h$_hs_bytestring_digits[x_tmp.subtract(x.multiply(b10))];
    } while(x.signum() !== 0);
    // reverse written digits;
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    h$ret1 = next_free;
    return buf_d;
}
// Padded, decimal, positive integers for the decimal output of bignums
///////////////////////////////////////////////////////////////////////
// Padded (9 digits), decimal, positive int:
// We will use it with numbers that fit in 31 bits; i.e., numbers smaller than
// 10^9, as "31 * log 2 / log 10 = 9.33"
function h$_hs_bytestring_int_dec_padded9(x, buf_d, buf_o) {
    var max_width_int32_dec = 9;
    var ptr = buf_o + max_width_int32_dec;
    var bu8 = buf_d.u8;
    var x_tmp;
    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[--ptr] = h$_hs_bytestring_digits[x_tmp - x * 10];
    } while(x);
    // pad beginning
    while (buf_o < ptr) { bu8[--ptr] = 48; }
}
// Padded (19 digits), decimal, positive long long int:
// We will use it with numbers that fit in 63 bits; i.e., numbers smaller than
// 10^18, as "63 * log 2 / log 10 = 18.96"
function h$_hs_bytestring_long_long_int_dec_padded18(x_a, x_b, buf_d, buf_o) {
    var l10 = h$_hs_bytestring_l10;
    var max_width_int64_dec = 18;
    var ptr = buf_o + max_width_int64_dec;
    var bu8 = buf_d.u8;
    var x = goog.math.Long.fromBits(x_b, x_a);
    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x = x.div(l10);
        bu8[--ptr] = h$_hs_bytestring_digits[x_tmp.subtract(x.multiply(l10))];
    } while (!x.isZero());
    // pad beginning
    while (buf_o < ptr) { bu8[--ptr] = 48; }
}
///////////////////////
// Hexadecimal encoding
///////////////////////
// unsigned ints (32 bit words)
function h$_hs_bytestring_uint_hex(x, buf_d, buf_o) {
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    // write hex representation in reverse order
    do {
        bu8[ptr++] = h$_hs_bytestring_digits[x & 0xf];
        x >>>= 4;
    } while(x);
    // invert written digits
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    h$ret1 = next_free;
    return buf_d;
}
// unsigned long ints (64 bit words)
function h$_hs_bytestring_long_long_uint_hex(x_a, x_b, buf_d, buf_o) {
    // write hex representation in reverse order
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    if(x_a === 0 && x_b === 0) {
        bu8[ptr++] = 48; // '0'
    } else {
        while(x_b !== 0) {
            bu8[ptr++] = h$_hs_bytestring_digits[x_b & 0xf];
            x_b >>>= 4;
        }
        while(x_a !== 0) {
            bu8[ptr++] = h$_hs_bytestring_digits[x_a & 0xf];
            x_a >>>= 4;
        }
    }
    // invert written digits
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    h$ret1 = next_free;
    return buf_d;
}
/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
// Copyright 2011 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
/**
 * @fileoverview Abstract cryptographic hash interface.
 *
 * See goog.crypt.Sha1 and goog.crypt.Md5 for sample implementations.
 *
 */
goog.provide('goog.crypt.Hash');
/**
 * Create a cryptographic hash instance.
 *
 * @constructor
 * @struct
 */
goog.crypt.Hash = function() {
  /**
   * The block size for the hasher.
   * @type {number}
   */
  this.blockSize = -1;
};
/**
 * Resets the internal accumulator.
 */
goog.crypt.Hash.prototype.reset = goog.abstractMethod;
/**
 * Adds a byte array (array with values in [0-255] range) or a string (might
 * only contain 8-bit, i.e., Latin1 characters) to the internal accumulator.
 *
 * Many hash functions operate on blocks of data and implement optimizations
 * when a full chunk of data is readily available. Hence it is often preferable
 * to provide large chunks of data (a kilobyte or more) than to repeatedly
 * call the update method with few tens of bytes. If this is not possible, or
 * not feasible, it might be good to provide data in multiplies of hash block
 * size (often 64 bytes). Please see the implementation and performance tests
 * of your favourite hash.
 *
 * @param {Array<number>|Uint8Array|string} bytes Data used for the update.
 * @param {number=} opt_length Number of bytes to use.
 */
goog.crypt.Hash.prototype.update = goog.abstractMethod;
/**
 * @return {!Array<number>} The finalized hash computed
 *     from the internal accumulator.
 */
goog.crypt.Hash.prototype.digest = goog.abstractMethod;
/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
// Copyright 2011 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
/**
 * @fileoverview MD5 cryptographic hash.
 * Implementation of http://tools.ietf.org/html/rfc1321 with common
 * optimizations and tweaks (see http://en.wikipedia.org/wiki/MD5).
 *
 * Usage:
 *   var md5 = new goog.crypt.Md5();
 *   md5.update(bytes);
 *   var hash = md5.digest();
 *
 * Performance:
 *   Chrome 23              ~680 Mbit/s
 *   Chrome 13 (in a VM)    ~250 Mbit/s
 *   Firefox 6.0 (in a VM)  ~100 Mbit/s
 *   IE9 (in a VM)           ~27 Mbit/s
 *   Firefox 3.6             ~15 Mbit/s
 *   IE8 (in a VM)           ~13 Mbit/s
 *
 */
goog.provide('goog.crypt.Md5');
goog.require('goog.crypt.Hash');
/**
 * MD5 cryptographic hash constructor.
 * @constructor
 * @extends {goog.crypt.Hash}
 * @final
 * @struct
 */
goog.crypt.Md5 = function() {
  goog.crypt.Md5.base(this, 'constructor');
  this.blockSize = 512 / 8;
  /**
   * Holds the current values of accumulated A-D variables (MD buffer).
   * @type {!Array<number>}
   * @private
   */
  this.chain_ = new Array(4);
  /**
   * A buffer holding the data until the whole block can be processed.
   * @type {!Array<number>}
   * @private
   */
  this.block_ = new Array(this.blockSize);
  /**
   * The length of yet-unprocessed data as collected in the block.
   * @type {number}
   * @private
   */
  this.blockLength_ = 0;
  /**
   * The total length of the message so far.
   * @type {number}
   * @private
   */
  this.totalLength_ = 0;
  this.reset();
};
goog.inherits(goog.crypt.Md5, goog.crypt.Hash);
/**
 * Integer rotation constants used by the abbreviated implementation.
 * They are hardcoded in the unrolled implementation, so it is left
 * here commented out.
 * @type {Array<number>}
 * @private
 *
goog.crypt.Md5.S_ = [
  7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
  5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
  4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
  6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
];
 */
/**
 * Sine function constants used by the abbreviated implementation.
 * They are hardcoded in the unrolled implementation, so it is left
 * here commented out.
 * @type {Array<number>}
 * @private
 *
goog.crypt.Md5.T_ = [
  0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
  0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
  0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
  0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
  0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
  0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
  0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
  0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
  0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
  0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
  0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
  0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
  0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
  0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
  0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
  0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
];
 */
/** @override */
goog.crypt.Md5.prototype.reset = function() {
  this.chain_[0] = 0x67452301;
  this.chain_[1] = 0xefcdab89;
  this.chain_[2] = 0x98badcfe;
  this.chain_[3] = 0x10325476;
  this.blockLength_ = 0;
  this.totalLength_ = 0;
};
/**
 * Internal compress helper function. It takes a block of data (64 bytes)
 * and updates the accumulator.
 * @param {Array<number>|Uint8Array|string} buf The block to compress.
 * @param {number=} opt_offset Offset of the block in the buffer.
 * @private
 */
goog.crypt.Md5.prototype.compress_ = function(buf, opt_offset) {
  if (!opt_offset) {
    opt_offset = 0;
  }
  // We allocate the array every time, but it's cheap in practice.
  var X = new Array(16);
  // Get 16 little endian words. It is not worth unrolling this for Chrome 11.
  if (goog.isString(buf)) {
    for (var i = 0; i < 16; ++i) {
      X[i] = (buf.charCodeAt(opt_offset++)) |
             (buf.charCodeAt(opt_offset++) << 8) |
             (buf.charCodeAt(opt_offset++) << 16) |
             (buf.charCodeAt(opt_offset++) << 24);
    }
  } else {
    for (var i = 0; i < 16; ++i) {
      X[i] = (buf[opt_offset++]) |
             (buf[opt_offset++] << 8) |
             (buf[opt_offset++] << 16) |
             (buf[opt_offset++] << 24);
    }
  }
  var A = this.chain_[0];
  var B = this.chain_[1];
  var C = this.chain_[2];
  var D = this.chain_[3];
  var sum = 0;
  /*
   * This is an abbreviated implementation, it is left here commented out for
   * reference purposes. See below for an unrolled version in use.
   *
  var f, n, tmp;
  for (var i = 0; i < 64; ++i) {

    if (i < 16) {
      f = (D ^ (B & (C ^ D)));
      n = i;
    } else if (i < 32) {
      f = (C ^ (D & (B ^ C)));
      n = (5 * i + 1) % 16;
    } else if (i < 48) {
      f = (B ^ C ^ D);
      n = (3 * i + 5) % 16;
    } else {
      f = (C ^ (B | (~D)));
      n = (7 * i) % 16;
    }

    tmp = D;
    D = C;
    C = B;
    sum = (A + f + goog.crypt.Md5.T_[i] + X[n]) & 0xffffffff;
    B += ((sum << goog.crypt.Md5.S_[i]) & 0xffffffff) |
         (sum >>> (32 - goog.crypt.Md5.S_[i]));
    A = tmp;
  }
   */
  /*
   * This is an unrolled MD5 implementation, which gives ~30% speedup compared
   * to the abbreviated implementation above, as measured on Chrome 11. It is
   * important to keep 32-bit croppings to minimum and inline the integer
   * rotation.
   */
  sum = (A + (D ^ (B & (C ^ D))) + X[0] + 0xd76aa478) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[1] + 0xe8c7b756) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[2] + 0x242070db) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[3] + 0xc1bdceee) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[4] + 0xf57c0faf) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[5] + 0x4787c62a) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[6] + 0xa8304613) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[7] + 0xfd469501) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[8] + 0x698098d8) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[9] + 0x8b44f7af) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[10] + 0xffff5bb1) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[11] + 0x895cd7be) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[12] + 0x6b901122) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[13] + 0xfd987193) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[14] + 0xa679438e) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[15] + 0x49b40821) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (C ^ (D & (B ^ C))) + X[1] + 0xf61e2562) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[6] + 0xc040b340) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[11] + 0x265e5a51) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[0] + 0xe9b6c7aa) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[5] + 0xd62f105d) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[10] + 0x02441453) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[15] + 0xd8a1e681) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[4] + 0xe7d3fbc8) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[9] + 0x21e1cde6) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[14] + 0xc33707d6) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[3] + 0xf4d50d87) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[8] + 0x455a14ed) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[13] + 0xa9e3e905) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[2] + 0xfcefa3f8) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[7] + 0x676f02d9) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[12] + 0x8d2a4c8a) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (B ^ C ^ D) + X[5] + 0xfffa3942) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[8] + 0x8771f681) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[11] + 0x6d9d6122) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[14] + 0xfde5380c) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[1] + 0xa4beea44) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[4] + 0x4bdecfa9) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[7] + 0xf6bb4b60) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[10] + 0xbebfbc70) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[13] + 0x289b7ec6) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[0] + 0xeaa127fa) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[3] + 0xd4ef3085) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[6] + 0x04881d05) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[9] + 0xd9d4d039) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[12] + 0xe6db99e5) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[15] + 0x1fa27cf8) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[2] + 0xc4ac5665) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (C ^ (B | (~D))) + X[0] + 0xf4292244) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[7] + 0x432aff97) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[14] + 0xab9423a7) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[5] + 0xfc93a039) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[12] + 0x655b59c3) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[3] + 0x8f0ccc92) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[10] + 0xffeff47d) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[1] + 0x85845dd1) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[8] + 0x6fa87e4f) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[15] + 0xfe2ce6e0) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[6] + 0xa3014314) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[13] + 0x4e0811a1) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[4] + 0xf7537e82) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[11] + 0xbd3af235) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[2] + 0x2ad7d2bb) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[9] + 0xeb86d391) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  this.chain_[0] = (this.chain_[0] + A) & 0xffffffff;
  this.chain_[1] = (this.chain_[1] + B) & 0xffffffff;
  this.chain_[2] = (this.chain_[2] + C) & 0xffffffff;
  this.chain_[3] = (this.chain_[3] + D) & 0xffffffff;
};
/** @override */
goog.crypt.Md5.prototype.update = function(bytes, opt_length) {
  if (!goog.isDef(opt_length)) {
    opt_length = bytes.length;
  }
  var lengthMinusBlock = opt_length - this.blockSize;
  // Copy some object properties to local variables in order to save on access
  // time from inside the loop (~10% speedup was observed on Chrome 11).
  var block = this.block_;
  var blockLength = this.blockLength_;
  var i = 0;
  // The outer while loop should execute at most twice.
  while (i < opt_length) {
    // When we have no data in the block to top up, we can directly process the
    // input buffer (assuming it contains sufficient data). This gives ~30%
    // speedup on Chrome 14 and ~70% speedup on Firefox 6.0, but requires that
    // the data is provided in large chunks (or in multiples of 64 bytes).
    if (blockLength == 0) {
      while (i <= lengthMinusBlock) {
        this.compress_(bytes, i);
        i += this.blockSize;
      }
    }
    if (goog.isString(bytes)) {
      while (i < opt_length) {
        block[blockLength++] = bytes.charCodeAt(i++);
        if (blockLength == this.blockSize) {
          this.compress_(block);
          blockLength = 0;
          // Jump to the outer loop so we use the full-block optimization.
          break;
        }
      }
    } else {
      while (i < opt_length) {
        block[blockLength++] = bytes[i++];
        if (blockLength == this.blockSize) {
          this.compress_(block);
          blockLength = 0;
          // Jump to the outer loop so we use the full-block optimization.
          break;
        }
      }
    }
  }
  this.blockLength_ = blockLength;
  this.totalLength_ += opt_length;
};
/** @override */
goog.crypt.Md5.prototype.digest = function() {
  // This must accommodate at least 1 padding byte (0x80), 8 bytes of
  // total bitlength, and must end at a 64-byte boundary.
  var pad = new Array((this.blockLength_ < 56 ?
                       this.blockSize :
                       this.blockSize * 2) - this.blockLength_);
  // Add padding: 0x80 0x00*
  pad[0] = 0x80;
  for (var i = 1; i < pad.length - 8; ++i) {
    pad[i] = 0;
  }
  // Add the total number of bits, little endian 64-bit integer.
  var totalBits = this.totalLength_ * 8;
  for (var i = pad.length - 8; i < pad.length; ++i) {
    pad[i] = totalBits & 0xff;
    totalBits /= 0x100; // Don't use bit-shifting here!
  }
  this.update(pad);
  var digest = new Array(16);
  var n = 0;
  for (var i = 0; i < 4; ++i) {
    for (var j = 0; j < 32; j += 8) {
      digest[n++] = (this.chain_[i] >>> j) & 0xff;
    }
  }
  return digest;
};
/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
/* include/HsBaseConfig.h.  Generated from HsBaseConfig.h.in by configure.  */
/* include/HsBaseConfig.h.in.  Generated from configure.ac by autoheader.  */
/* The value of E2BIG. */
/* The value of EACCES. */
/* The value of EADDRINUSE. */
/* The value of EADDRNOTAVAIL. */
/* The value of EADV. */
/* The value of EAFNOSUPPORT. */
/* The value of EAGAIN. */
/* The value of EALREADY. */
/* The value of EBADF. */
/* The value of EBADMSG. */
/* The value of EBADRPC. */
/* The value of EBUSY. */
/* The value of ECHILD. */
/* The value of ECOMM. */
/* The value of ECONNABORTED. */
/* The value of ECONNREFUSED. */
/* The value of ECONNRESET. */
/* The value of EDEADLK. */
/* The value of EDESTADDRREQ. */
/* The value of EDIRTY. */
/* The value of EDOM. */
/* The value of EDQUOT. */
/* The value of EEXIST. */
/* The value of EFAULT. */
/* The value of EFBIG. */
/* The value of EFTYPE. */
/* The value of EHOSTDOWN. */
/* The value of EHOSTUNREACH. */
/* The value of EIDRM. */
/* The value of EILSEQ. */
/* The value of EINPROGRESS. */
/* The value of EINTR. */
/* The value of EINVAL. */
/* The value of EIO. */
/* The value of EISCONN. */
/* The value of EISDIR. */
/* The value of ELOOP. */
/* The value of EMFILE. */
/* The value of EMLINK. */
/* The value of EMSGSIZE. */
/* The value of EMULTIHOP. */
/* The value of ENAMETOOLONG. */
/* The value of ENETDOWN. */
/* The value of ENETRESET. */
/* The value of ENETUNREACH. */
/* The value of ENFILE. */
/* The value of ENOBUFS. */
/* The value of ENOCIGAR. */
/* The value of ENODATA. */
/* The value of ENODEV. */
/* The value of ENOENT. */
/* The value of ENOEXEC. */
/* The value of ENOLCK. */
/* The value of ENOLINK. */
/* The value of ENOMEM. */
/* The value of ENOMSG. */
/* The value of ENONET. */
/* The value of ENOPROTOOPT. */
/* The value of ENOSPC. */
/* The value of ENOSR. */
/* The value of ENOSTR. */
/* The value of ENOSYS. */
/* The value of ENOTBLK. */
/* The value of ENOTCONN. */
/* The value of ENOTDIR. */
/* The value of ENOTEMPTY. */
/* The value of ENOTSOCK. */
/* The value of ENOTSUP. */
/* The value of ENOTTY. */
/* The value of ENXIO. */
/* The value of EOPNOTSUPP. */
/* The value of EPERM. */
/* The value of EPFNOSUPPORT. */
/* The value of EPIPE. */
/* The value of EPROCLIM. */
/* The value of EPROCUNAVAIL. */
/* The value of EPROGMISMATCH. */
/* The value of EPROGUNAVAIL. */
/* The value of EPROTO. */
/* The value of EPROTONOSUPPORT. */
/* The value of EPROTOTYPE. */
/* The value of ERANGE. */
/* The value of EREMCHG. */
/* The value of EREMOTE. */
/* The value of EROFS. */
/* The value of ERPCMISMATCH. */
/* The value of ERREMOTE. */
/* The value of ESHUTDOWN. */
/* The value of ESOCKTNOSUPPORT. */
/* The value of ESPIPE. */
/* The value of ESRCH. */
/* The value of ESRMNT. */
/* The value of ESTALE. */
/* The value of ETIME. */
/* The value of ETIMEDOUT. */
/* The value of ETOOMANYREFS. */
/* The value of ETXTBSY. */
/* The value of EUSERS. */
/* The value of EWOULDBLOCK. */
/* The value of EXDEV. */
/* The value of O_BINARY. */
/* The value of SIGINT. */
/* Define to 1 if you have the `clock_gettime' function. */
/* #undef HAVE_CLOCK_GETTIME */
/* Define to 1 if you have the <ctype.h> header file. */
/* Define if you have epoll support. */
/* #undef HAVE_EPOLL */
/* Define to 1 if you have the `epoll_ctl' function. */
/* #undef HAVE_EPOLL_CTL */
/* Define to 1 if you have the <errno.h> header file. */
/* Define to 1 if you have the `eventfd' function. */
/* #undef HAVE_EVENTFD */
/* Define to 1 if you have the <fcntl.h> header file. */
/* Define to 1 if you have the `ftruncate' function. */
/* Define to 1 if you have the `getclock' function. */
/* #undef HAVE_GETCLOCK */
/* Define to 1 if you have the `getrusage' function. */
/* Define to 1 if you have the <inttypes.h> header file. */
/* Define to 1 if you have the `iswspace' function. */
/* Define to 1 if you have the `kevent' function. */
/* Define to 1 if you have the `kevent64' function. */
/* Define if you have kqueue support. */
/* Define to 1 if you have the <langinfo.h> header file. */
/* Define to 1 if you have libcharset. */
/* Define to 1 if you have the `rt' library (-lrt). */
/* #undef HAVE_LIBRT */
/* Define to 1 if you have the <limits.h> header file. */
/* Define to 1 if the system has the type `long long'. */
/* Define to 1 if you have the `lstat' function. */
/* Define to 1 if you have the <memory.h> header file. */
/* Define if you have poll support. */
/* Define to 1 if you have the <poll.h> header file. */
/* Define to 1 if you have the <signal.h> header file. */
/* Define to 1 if you have the <stdint.h> header file. */
/* Define to 1 if you have the <stdlib.h> header file. */
/* Define to 1 if you have the <strings.h> header file. */
/* Define to 1 if you have the <string.h> header file. */
/* Define to 1 if you have the <sys/epoll.h> header file. */
/* #undef HAVE_SYS_EPOLL_H */
/* Define to 1 if you have the <sys/eventfd.h> header file. */
/* #undef HAVE_SYS_EVENTFD_H */
/* Define to 1 if you have the <sys/event.h> header file. */
/* Define to 1 if you have the <sys/resource.h> header file. */
/* Define to 1 if you have the <sys/select.h> header file. */
/* Define to 1 if you have the <sys/stat.h> header file. */
/* Define to 1 if you have the <sys/syscall.h> header file. */
/* Define to 1 if you have the <sys/timeb.h> header file. */
/* Define to 1 if you have the <sys/timers.h> header file. */
/* #undef HAVE_SYS_TIMERS_H */
/* Define to 1 if you have the <sys/times.h> header file. */
/* Define to 1 if you have the <sys/time.h> header file. */
/* Define to 1 if you have the <sys/types.h> header file. */
/* Define to 1 if you have the <sys/utsname.h> header file. */
/* Define to 1 if you have the <sys/wait.h> header file. */
/* Define to 1 if you have the <termios.h> header file. */
/* Define to 1 if you have the `times' function. */
/* Define to 1 if you have the <time.h> header file. */
/* Define to 1 if you have the <unistd.h> header file. */
/* Define to 1 if you have the <utime.h> header file. */
/* Define to 1 if you have the <wctype.h> header file. */
/* Define to 1 if you have the <windows.h> header file. */
/* #undef HAVE_WINDOWS_H */
/* Define to 1 if you have the <winsock.h> header file. */
/* #undef HAVE_WINSOCK_H */
/* Define to 1 if you have the `_chsize' function. */
/* #undef HAVE__CHSIZE */
/* Define to Haskell type for cc_t */
/* Define to Haskell type for char */
/* Define to Haskell type for clock_t */
/* Define to Haskell type for dev_t */
/* Define to Haskell type for double */
/* Define to Haskell type for float */
/* Define to Haskell type for gid_t */
/* Define to Haskell type for ino_t */
/* Define to Haskell type for int */
/* Define to Haskell type for intmax_t */
/* Define to Haskell type for intptr_t */
/* Define to Haskell type for long */
/* Define to Haskell type for long long */
/* Define to Haskell type for mode_t */
/* Define to Haskell type for nlink_t */
/* Define to Haskell type for off_t */
/* Define to Haskell type for pid_t */
/* Define to Haskell type for ptrdiff_t */
/* Define to Haskell type for rlim_t */
/* Define to Haskell type for short */
/* Define to Haskell type for signed char */
/* Define to Haskell type for sig_atomic_t */
/* Define to Haskell type for size_t */
/* Define to Haskell type for speed_t */
/* Define to Haskell type for ssize_t */
/* Define to Haskell type for suseconds_t */
/* Define to Haskell type for tcflag_t */
/* Define to Haskell type for time_t */
/* Define to Haskell type for uid_t */
/* Define to Haskell type for uintmax_t */
/* Define to Haskell type for uintptr_t */
/* Define to Haskell type for unsigned char */
/* Define to Haskell type for unsigned int */
/* Define to Haskell type for unsigned long */
/* Define to Haskell type for unsigned long long */
/* Define to Haskell type for unsigned short */
/* Define to Haskell type for useconds_t */
/* Define to Haskell type for wchar_t */
/* Define to the address where bug reports for this package should be sent. */
/* Define to the full name of this package. */
/* Define to the full name and version of this package. */
/* Define to the one symbol short name of this package. */
/* Define to the home page for this package. */
/* Define to the version of this package. */
/* The size of `kev.filter', as computed by sizeof. */
/* The size of `kev.flags', as computed by sizeof. */
/* The size of `struct MD5Context', as computed by sizeof. */
/* Define to 1 if you have the ANSI C header files. */
/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */
/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */
// #define GHCJS_TRACE_IO 1
function h$base_access(file, file_off, mode, c) {
    ;
    if(h$isNode) {
        h$fs.stat(fd, function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                c(mode & fs.mode); // fixme is this ok?
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_chmod(file, file_off, mode, c) {
    ;
    if(h$isNode) {
        h$fs.chmod(h$decodeUtf8z(file, file_off), mode, function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_close(fd, c) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo && fdo.close) {
        fdo.close(fd, fdo, c);
    } else {
        h$errno = 22;
        c(-1);
    }
}
function h$base_dup(fd, something, c) {
    throw "h$base_dup";
}
function h$base_dup2(fd, c) {
    throw "h$base_dup2";
}
function h$base_fstat(fd, stat, stat_off, c) {
    ;
    if(h$isNode) {
        h$fs.fstat(fd, function(err, fs) {
            if(err) {
                h$handlErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_isatty(fd) {
    ;
    if(h$isNode) {
        if(fd === 0) return process.stdin.isTTY?1:0;
        if(fd === 1) return process.stdout.isTTY?1:0;
        if(fd === 2) return process.stderr.isTTY?1:0;
    }
    if(fd === 1 || fd === 2) return 1;
    return 0;
}
function h$base_lseek(fd, pos_1, pos_2, whence, c) {
    ;
    if(h$isNode) {
        var p = goog.math.Long.fromBits(pos_2, pos_1), p1;
        var o = h$base_fds[fd];
        if(!o) {
            h$errno = CONST_BADF;
            c(-1,-1);
        } else {
            switch(whence) {
            case 0: /* SET */
                o.pos = p.toNumber();
                c(p.getHighBits(), p.getLowBits());
                break;
            case 1: /* CUR */
                o.pos += p.toNumber();
                p1 = goog.math.Long.fromNumber(o.pos);
                c(p1.getHighBits(), p1.getLowBits());
                break;
            case 2: /* END */
                h$fs.fstat(fd, function(err, fs) {
                    if(err) {
                        h$setErrno(err);
                        c(-1,-1);
                    } else {
                        o.pos = fs.size + p.toNumber();
                        p1 = goog.math.Long.fromNumber(o.pos);
                        c(p1.getHighBits(), p1.getLowBits());
                    }
                });
                break;
            default:
                h$errno = 22;
                c(-1,-1);
            }
        }
    } else {
        h$unsupported();
        c(-1, -1);
    }
}
function h$base_lstat(file, file_off, stat, stat_off, c) {
    ;
    if(h$isNode) {
        h$fs.lstat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_open(file, file_off, how, mode, c) {
    if(h$isNode) {
        var flags, off;
        var fp = h$decodeUtf8z(file, file_off);
        var acc = how & h$base_o_accmode;
        // passing a number lets node.js use it directly as the flags (undocumented)
        if(acc === h$base_o_rdonly) {
            flags = h$processConstants['O_RDONLY'];
        } else if(acc === h$base_o_wronly) {
            flags = h$processConstants['O_WRONLY'];
        } else { // r+w
            flags = h$processConstants['O_RDWR'];
        }
        off = (how & h$base_o_append) ? -1 : 0;
        flags = flags | ((how & h$base_o_trunc) ? h$processConstants['O_TRUNC'] : 0)
                      | ((how & h$base_o_creat) ? h$processConstants['O_CREAT'] : 0)
                      | ((how & h$base_o_excl) ? h$processConstants['O_EXCL'] : 0)
                      | ((how & h$base_o_append) ? h$processConstants['O_APPEND'] : 0);
        h$fs.open(fp, flags, mode, function(err, fd) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                var f = function(p) {
                    h$base_fds[fd] = { read: h$base_readFile
                                       , write: h$base_writeFile
                                       , close: h$base_closeFile
                                       , pos: p
                                     };
                    c(fd);
                }
                if(off === -1) {
                    h$fs.stat(fp, function(err, fs) {
                        if(err) h$handleErrnoC(err, -1, 0, c); else f(fs.size);
                    });
                } else {
                    f(0);
                }
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_read(fd, buf, buf_off, n, c) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo && fdo.read) {
        fdo.read(fd, fdo, buf, buf_off, n, c);
    } else {
        h$errno = 22;
        c(-1);
    }
}
function h$base_stat(file, file_off, stat, stat_off, c) {
    ;
    if(h$isNode) {
        h$fs.stat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handlErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_umask(mode) {
    ;
    if(h$isNode) return process.umask(mode);
    return 0;
}
function h$base_write(fd, buf, buf_off, n, c) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo && fdo.write) {
        fdo.write(fd, fdo, buf, buf_off, n, c);
    } else {
        h$errno = 22;
        c(-1);
    }
}
function h$base_ftruncate(fd, pos_1, pos_2, c) {
    ;
    if(h$isNode) {
        h$fs.ftruncate(fd, goog.math.Long.fromBits(pos_2, pos_1).toNumber(), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_unlink(file, file_off, c) {
    ;
    if(h$isNode) {
        h$fs.unlink(h$decodeUtf8z(file, file_off), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_getpid() {
    ;
    if(h$isNode) return process.pid;
    return 0;
}
function h$base_link(file1, file1_off, file2, file2_off, c) {
    ;
    if(h$isNode) {
        h$fs.link(h$decodeUtf8z(file1, file1_off), h$decodeUtf8z(file2, file2_off), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_mkfifo(file, file_off, mode, c) {
    throw "h$base_mkfifo";
}
function h$base_sigemptyset(sigset, sigset_off) {
    return 0;
    // throw "h$base_sigemptyset";
}
function h$base_sigaddset(sigset, sigset_off, sig) {
    return 0;
    // throw "h$base_sigaddset";
}
function h$base_sigprocmask(sig, sigset1, sigset1_off, sigset2, sigset2_off) {
    return 0;
    // throw "h$base_sigprocmask";
}
function h$base_tcgetattr(attr, termios, termios_off) {
    return 0;
}
function h$base_tcsetattr(attr, val, termios, termios_off) {
    return 0;
}
function h$base_utime(file, file_off, timbuf, timbuf_off, c) {
    ;
    if(h$isNode) {
        h$fs.fstat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handleErrnoC(err, 0, -1, c); // fixme
            } else {
                var atime = goog.math.Long.fromNumber(fs.atime.getTime());
                var mtime = goog.math.Long.fromNumber(fs.mtime.getTime());
                var ctime = goog.math.Long.fromNumber(fs.ctime.getTime());
                timbuf.i3[0] = atime.getHighBits();
                timbuf.i3[1] = atime.getLowBits();
                timbuf.i3[2] = mtime.getHighBits();
                timbuf.i3[3] = mtime.getLowBits();
                timbuf.i3[4] = ctime.getHighBits();
                timbuf.i3[5] = ctime.getLowBits();
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_waitpid(pid, stat, stat_off, options, c) {
    throw "h$base_waitpid";
}
/** @const */ var h$base_o_rdonly = 0x00000;
/** @const */ var h$base_o_wronly = 0x00001;
/** @const */ var h$base_o_rdwr = 0x00002;
/** @const */ var h$base_o_accmode = 0x00003;
/** @const */ var h$base_o_append = 0x00008;
/** @const */ var h$base_o_creat = 0x00200;
/** @const */ var h$base_o_trunc = 0x00400;
/** @const */ var h$base_o_excl = 0x00800;
/** @const */ var h$base_o_noctty = 0x20000;
/** @const */ var h$base_o_nonblock = 0x00004;
/** @const */ var h$base_o_binary = 0x00000;
function h$base_c_s_isreg(mode) {
    return 1;
}
function h$base_c_s_ischr(mode) {
    return 0;
}
function h$base_c_s_isblk(mode) {
    return 0;
}
function h$base_c_s_isdir(mode) {
    return 0; // fixme
}
function h$base_c_s_isfifo(mode) {
    return 0;
}
function h$base_fillStat(fs, b, off) {
    if(off%4) throw "h$base_fillStat: not aligned";
    var o = off>>2;
    b.i3[o+0] = fs.mode;
    var s = goog.math.Long.fromNumber(fs.size);
    b.i3[o+1] = s.getHighBits();
    b.i3[o+2] = s.getLowBits();
    b.i3[o+3] = 0; // fixme
    b.i3[o+4] = 0; // fixme
    b.i3[o+5] = fs.dev;
    var i = goog.math.Long.fromNumber(fs.ino);
    b.i3[o+6] = i.getHighBits();
    b.i3[o+7] = i.getLowBits();
    b.i3[o+8] = fs.uid;
    b.i3[o+9] = fs.gid;
}
// [mode,size1,size2,mtime1,mtime2,dev,ino1,ino2,uid,gid] all 32 bit
/** @const */ var h$base_sizeof_stat = 40;
function h$base_st_mtime(stat, stat_off) {
    h$ret1 = stat.i3[(stat_off>>2)+4];
    return stat.i3[(stat_off>>2)+3];
}
function h$base_st_size(stat, stat_off) {
    h$ret1 = stat.i3[(stat_off>>2)+2];
    return stat.i3[(stat_off>>2)+1];
}
function h$base_st_mode(stat, stat_off) {
    return stat.i3[stat_off>>2];
}
function h$base_st_dev(stat, stat_off) {
    return stat.i3[(stat_off>>2)+5];
}
function h$base_st_ino(stat, stat_off) {
    h$ret1 = stat.i3[(stat_off>>2)+7];
    return stat.i3[(stat_off>>2)+6];
}
/** @const */ var h$base_echo = 1;
/** @const */ var h$base_tcsanow = 2;
/** @const */ var h$base_icanon = 4;
/** @const */ var h$base_vmin = 8;
/** @const */ var h$base_vtime = 16;
/** @const */ var h$base_sigttou = 0;
/** @const */ var h$base_sig_block = 0;
/** @const */ var h$base_sig_setmask = 0;
/** @const */ var h$base_f_getfl = 0;
/** @const */ var h$base_f_setfl = 0;
/** @const */ var h$base_f_setfd = 0;
/** @const */ var h$base_fd_cloexec = 0;
/** @const */ var h$base_sizeof_termios = 4;
/** @const */ var h$base_sizeof_sigset_t = 4;
/** @const */ var h$base_lflag = 0;
function h$base_poke_lflag(termios, termios_off, flag) {
    return 0;
}
function h$base_ptr_c_cc(termios, termios_off) {
    h$ret1 = 0;
    return h$newByteArray(8);
}
/** @const */ var h$base_default_buffer_size = 32768;
function h$base_c_s_issock(mode) {
    return 0; // fixme
}
/** @const */ var h$base_SEEK_SET = 0;
/** @const */ var h$base_SEEK_CUR = 1;
/** @const */ var h$base_SEEK_END = 2;
function h$base_set_saved_termios(a, b, c) {
    h$ret1 = 0
    return null;
}
function h$base_get_saved_termios(r) {
    h$ret1 = 0;
    return null;
}
// fixme
function h$lockFile(fd, dev, ino, for_writing) {
    ;
    return 0;
}
function h$unlockFile(fd) {
    ;
    return 0;
}
// engine-dependent setup
var h$base_readStdin, h$base_writeStderr, h$base_writeStdout;
var h$base_readFile, h$base_writeFile, h$base_closeFile;
var h$base_stdin_waiting = new h$Queue();
var h$base_stdin_chunk = { buf: null
                           , pos: 0
                           , processing: false
                           };
var h$base_stdin_eof = false;
var h$base_process_stdin = function() {
    var c = h$base_stdin_chunk;
    var q = h$base_stdin_waiting;
    if(!q.length() || c.processing) return;
    c.processing = true;
    if(!c.buf) { c.pos = 0; c.buf = process.stdin.read(); }
    while(c.buf && q.length()) {
        var x = q.dequeue();
        var n = Math.min(c.buf.length - c.pos, x.n);
        for(var i=0;i<n;i++) {
            x.buf.u8[i+x.off] = c.buf[c.pos+i];
        }
        c.pos += n;
        x.c(n);
        if(c.pos >= c.buf.length) c.buf = null;
        if(!c.buf && q.length()) { c.pos = 0; c.buf = process.stdin.read(); }
    }
    while(h$base_stdin_eof && q.length()) q.dequeue().c(0);
    c.processing = false;
}
if(h$isNode) {
    h$base_closeFile = function(fd, fdo, c) {
        h$fs.close(fd, function(err) {
            delete h$base_fds[fd];
            h$handleErrnoC(err, -1, 0, c);
        });
    }
    h$base_readFile = function(fd, fdo, buf, buf_offset, n, c) {
        var pos = typeof fdo.pos === 'number' ? fdo.pos : null;
        ;
        h$fs.read(fd, new Buffer(n), 0, n, pos, function(err, bytesRead, nbuf) {
            if(err) {
                h$setErrno(err);
                c(-1);
            } else {
                for(var i=bytesRead-1;i>=0;i--) buf.u8[buf_offset+i] = nbuf[i];
                if(typeof fdo.pos === 'number') fdo.pos += bytesRead;
                c(bytesRead);
            }
        });
    }
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        ;
        h$base_stdin_waiting.enqueue({buf: buf, off: buf_offset, n: n, c: c});
        h$base_process_stdin();
    }
    h$base_writeFile = function(fd, fdo, buf, buf_offset, n, c) {
        var pos = typeof fdo.pos === 'number' ? fdo.pos : null;
        ;
        var nbuf = new Buffer(n);
        for(var i=0;i<n;i++) nbuf[i] = buf.u8[i+buf_offset];
        if(typeof fdo.pos === 'number') fdo.pos += n;
        h$fs.write(fd, nbuf, 0, n, pos, function(err, bytesWritten) {
            ;
            if(err) {
                h$setErrno(err);
                if(typeof fdo.pos === 'number') fdo.pos -= n;
                if(h$errno === 35)
                    setTimeout(function() { h$base_writeFile(fd, fdo, buf, buf_offset, n, c); }, 20);
                else c(-1);
            } else {
                if(typeof fdo.pos === 'number') fdo.pos += bytesWritten - n;
                c(bytesWritten);
            }
        });
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        ;
        h$base_writeFile(1, fdo, buf, buf_offset, n, c);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        ;
        h$base_writeFile(2, fdo, buf, buf_offset, n, c);
    }
    process.stdin.on('readable', h$base_process_stdin);
    process.stdin.on('end', function() { h$base_stdin_eof = true; h$base_process_stdin(); });
} else if (h$isJsShell) {
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        c(0);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        putstr(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        printErr(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
} else if(h$isJsCore) {
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
 c(0);
    }
    var h$base_stdoutLeftover = { f: print, val: null };
    var h$base_stderrLeftover = { f: debug, val: null };
    var h$base_writeWithLeftover = function(buf, n, buf_offset, c, lo) {
 var lines = h$decodeUtf8(buf, n, buf_offset).split(/\r?\n/);
 if(lines.length === 1) {
     if(lines[0].length) {
  if(lo.val !== null) lo.val += lines[0];
  else lo.val = lines[0];
     }
 } else {
            lo.f(((lo.val !== null) ? lo.val : '') + lines[0]);
     for(var i=1;i<lines.length-1;i++) lo.f(lines[i]);
     if(lines[lines.length-1].length) lo.val = lines[lines.length-1];
     else lo.val = null;
 }
 c(n);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
 h$base_writeWithLeftover(buf, n, buf_offset, c, h$base_stdoutLeftover);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
 // writing to stderr not supported, write to stdout
 h$base_writeWithLeftover(buf, n, buf_offset, c, h$base_stderrLeftover);
    }
} else { // browser / fallback
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        c(0);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        console.log(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        console.log(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
}
var h$base_stdin_fd = { read: h$base_readStdin };
var h$base_stdout_fd = { write: h$base_writeStdout };
var h$base_stderr_fd = { write: h$base_writeStderr };
var h$base_fdN = -1; // negative file descriptors are 'virtual'
var h$base_fds = [h$base_stdin_fd, h$base_stdout_fd, h$base_stderr_fd];
function h$shutdownHaskellAndExit(code, fast) {
    if(h$isNode) {
        process.exit(code);
    } else if(h$isJsShell) {
        quit(code);
    } else if(h$isJsCore) {
 if(h$base_stdoutLeftover.val !== null) print(h$base_stdoutLeftover.val);
 if(h$base_stderrLeftover.val !== null) debug(h$base_stderrLeftover.val);
 // jsc does not support returning a nonzero value, print it instead
 if(code !== 0) debug("GHCJS JSC exit status: " + code);
 quit();
    }
}
// RAND_MAX = 32767
function h$rand() {
  return (32768 * Math.random()) & 32767;
}
/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
// JS Objects stuff
function h$isFloat (n) {
  return n===+n && n!==(n|0);
}
function h$isInteger (n) {
  return n===+n && n===(n|0);
}
/*
        -- 0 - null, 1 - integer,
        -- 2 - float, 3 - bool,
        -- 4 - string, 5 - array
        -- 6 - object
*/
function h$typeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) {
            return 0;
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            // it's an array
            return 5;
        } else if (!o) {
            // null 
            return 0;
        } else {
            // it's an object
            return 6;
        }
    }
}
function h$listprops(o) {
    if (!(o instanceof Object)) {
        return [];
    }
    var l = [];
    for (var prop in o) {
        l.push(prop);
    }
    return l;
}
function h$flattenObj(o) {
    var l = [];
    for (var prop in o) {
        l.push([prop, o[prop]]);
    }
    return l;
}
/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
function h$ghcjs_currentWindow() {
  return window;
};
function h$ghcjs_currentDocument() {
  return document;
};
/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
/* FNV-1 hash
 *
 * The FNV-1 hash description: http://isthe.com/chongo/tech/comp/fnv/
 * The FNV-1 hash is public domain: http://isthe.com/chongo/tech/comp/fnv/#public_domain
 */
function h$hashable_fnv_hash_offset(str_a, o, len, hash) {
  return h$hashable_fnv_hash(str_a, o, len, hash);
}
function h$hashable_fnv_hash(str_d, str_o, len, hash) {
  if(len > 0) {
    var d = str_d.u8;
    for(var i=0;i<len;i++) {
      hash = h$mulInt32(hash, 16777619) ^ d[str_o+i];
    }
  }
  return hash;
}
// int hashable_getRandomBytes(unsigned char *dest, int nbytes)
function h$hashable_getRandomBytes(dest_d, dest_o, len) {
  if(len > 0) {
    var d = dest_d.u8;
    for(var i=0;i<len;i++) {
      d[dest_o+i] = Math.floor(Math.random() * 256);
    }
  }
  return len;
}
/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
function h$_hs_text_memcpy(dst_v,dst_o2,src_v,src_o2,n) {
  return h$memcpy(dst_v,2*dst_o2,src_v,2*src_o2,2*n);
}
function h$_hs_text_memcmp(a_v,a_o2,b_v,b_o2,n) {
  return h$memcmp(a_v,2*a_o2,b_v,2*b_o2,2*n);
}
// decoder below adapted from cbits/cbits.c in the text package
var h$_text_UTF8_ACCEPT = 0;
var h$_text_UTF8_REJECT = 12
var h$_text_utf8d =
   [
  /*
   * The first part of the table maps bytes to character classes that
   * to reduce the size of the transition table and create bitmasks.
   */
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,
  /*
   * The second part is a transition table that maps a combination of
   * a state of the automaton and a character class to a state.
   */
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12];
/*
 * A best-effort decoder. Runs until it hits either end of input or
 * the start of an invalid byte sequence.
 *
 * At exit, updates *destoff with the next offset to write to, and
 * returns the next source offset to read from.
 */
function h$_hs_text_decode_utf8_internal ( dest_v
                                         , destoff_v, destoff_o
                                         , src_v, src_o
                                         , src_end_v, src_end_o
                                         , s
                                         ) {
  if(src_v === null || src_end_v === null) {
    h$ret1 = src_end_o;
    return null;
  }
  var dsto = destoff_v.dv.getUint32(destoff_o,true) << 1;
  var srco = src_o;
  var state = s.state;
  var codepoint = s.codepoint;
  var ddv = dest_v.dv;
  var sdv = src_v.dv;
  function decode(b) {
    var type = h$_text_utf8d[b];
    codepoint = (state !== h$_text_UTF8_ACCEPT) ?
      (b & 0x3f) | (codepoint << 6) :
      (0xff >>> type) & b;
    state = h$_text_utf8d[256 + state + type];
    return state;
  }
  while (srco < src_end_o) {
    if(decode(sdv.getUint8(srco++)) !== h$_text_UTF8_ACCEPT) {
      if(state !== h$_text_UTF8_REJECT) {
        continue;
      } else {
        break;
      }
    }
    if (codepoint <= 0xffff) {
      ddv.setUint16(dsto,codepoint,true);
      dsto += 2;
    } else {
      ddv.setUint16(dsto,(0xD7C0 + (codepoint >>> 10)),true);
      ddv.setUint16(dsto+2,(0xDC00 + (codepoint & 0x3FF)),true);
      dsto += 4;
    }
    s.last = srco;
  }
  s.state = state;
  s.codepoint = codepoint;
  destoff_v.dv.setUint32(destoff_o,dsto>>1,true);
  h$ret1 = srco;
  return src_v;
}
function h$_hs_text_decode_utf8_state( dest_v
                                     , destoff_v, destoff_o
                                     , src_v, src_o
                                     , srcend_v, srcend_o
                                     , codepoint0_v, codepoint0_o
                                     , state0_v, state0_o
                                     ) {
  var s = { state: state0_v.dv.getUint32(state0_o, true)
          , codepoint: codepoint0_v.dv.getUint32(codepoint0_o, true)
          , last: src_o
          };
  var ret = h$_hs_text_decode_utf8_internal ( dest_v
                                            , destoff_v, destoff_o
                                            , src_v.arr[src_o][0], src_v.arr[src_o][1]
                                            , srcend_v, srcend_o
                                            , s
                                            );
  src_v.arr[src_o][1] = s.last;
  state0_v.dv.setUint32(state0_o, s.state, true);
  codepoint0_v.dv.setUint32(codepoint0_o, s.codepoint, true);
  if(s.state === h$_text_UTF8_REJECT)
    h$ret1--;
  return ret;
}
function h$_hs_text_decode_utf8( dest_v
                               , destoff_v, destoff_o
                               , src_v, src_o
                               , srcend_v, srcend_o
                               ) {
  /* Back up if we have an incomplete or invalid encoding */
  var s = { state: h$_text_UTF8_ACCEPT
          , codepoint: 0
          , last: src_o
          };
  var ret = h$_hs_text_decode_utf8_internal ( dest_v
                                            , destoff_v, destoff_o
                                            , src_v, src_o
                                            , srcend_v, srcend_o
                                            , s
                                            );
  if (s.state !== h$_text_UTF8_ACCEPT)
    h$ret1--;
  return ret;
}
/*
 * The ISO 8859-1 (aka latin-1) code points correspond exactly to the first 256 unicode
 * code-points, therefore we can trivially convert from a latin-1 encoded bytestring to
 * an UTF16 array
 */
function h$_hs_text_decode_latin1(dest_d, src_d, src_o, srcend_d, srcend_o) {
  var p = src_o;
  var d = 0;
  var su8 = src_d.u8;
  var su3 = src_d.u3;
  var du1 = dest_d.u1;
  // consume unaligned prefix
  while(p != srcend_o && p & 3) {
    du1[d++] = su8[p++];
  }
  // iterate over 32-bit aligned loads
  if(su3) {
    while (p < srcend_o - 3) {
      var w = su3[p>>2];
      du1[d++] = w & 0xff;
      du1[d++] = (w >>> 8) & 0xff;
      du1[d++] = (w >>> 16) & 0xff;
      du1[d++] = (w >>> 32) & 0xff;
      p += 4;
    }
  }
  // handle unaligned suffix
  while (p != srcend_o)
    du1[d++] = su8[p++];
}
function h$_hs_text_encode_utf8(destp_v, destp_o, src_v, srcoff, srclen) {
  var dest_v = destp_v.arr[destp_o][0];
  var dest_o = destp_v.arr[destp_o][1];
  var src = srcoff;
  var dest = dest_o;
  var srcend = src + srclen;
  var srcu1 = src_v.u1;
  if(!srcu1) throw "h$_hs_text_encode_utf8: invalid alignment for source";
  var srcu3 = src_v.u3;
  var destu8 = dest_v.u8;
  while(src < srcend) {
    // run of (aligned) ascii characters
    while(srcu3 && !(src & 1) && srcend - src >= 2) {
      var w = srcu3[src>>1];
      if(w & 0xFF80FF80) break;
      destu8[dest++] = w & 0xFFFF;
      destu8[dest++] = w >>> 16;
      src += 2;
    }
    while(src < srcend) {
      var w = srcu1[src++];
      if(w <= 0x7F) {
        destu8[dest++] = w;
        break; // go back to a stream of ASCII
      } else if(w <= 0x7FF) {
        destu8[dest++] = (w >> 6) | 0xC0;
        destu8[dest++] = (w & 0x3f) | 0x80;
      } else if(w < 0xD800 || w > 0xDBFF) {
        destu8[dest++] = (w >>> 12) | 0xE0;
        destu8[dest++] = ((w >> 6) & 0x3F) | 0x80;
        destu8[dest++] = (w & 0x3F) | 0x80;
      } else {
        var c = ((w - 0xD800) << 10) + (srcu1[src++] - 0xDC00) + 0x10000;
        destu8[dest++] = (c >>> 18) | 0xF0;
        destu8[dest++] = ((c >> 12) & 0x3F) | 0x80;
        destu8[dest++] = ((c >> 6) & 0x3F) | 0x80;
        destu8[dest++] = (c & 0x3F) | 0x80;
      }
    }
  }
  destp_v.arr[destp_o][1] = dest;
}
