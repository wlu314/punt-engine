const std = @import("std");

// not sure if you can use `comptime` to set these bits dynamically
pub fn Pattern(comptime PRICE_BITS: , comptime QUANTITY_BITS: comptime_int) type {
    // the 1 is the buy/sell flag
    const TOTAL_BITS = @as(u6, 1 + PRICE_BITS + QUANTITY_BITS);

    const DataType = switch (TOTAL_BITS) {

    }
}
// see what I'm trying to do though? see discord thread on the q/p stuff
