/// Pattern is the software side of our internal represenation of orders.
/// We have three parts:
///     - a one bit flag for buy/sell
///     - q bits for the quantity
///     - p bits for the limit price
///
/// The position of the decimal within q and p is implied.
///
/// With this struct defined, you'll still need to manually update other componenets
/// to match the pattern bit width. Consider this Verilog:
/// ```verilog
/// module rule_parser #(
///     parameter PRICE_BITS = 10
///     parameter QUANTITY_BITS = 9,
///     parameter TOTAL_BITS = 1 + PRICE_BITS + QUANTITY_BITS
/// ) (
///     input wire [TOTAL_BITS-1:0] rule_data,
///     output wire buy_sell,
///     output wire [PRICE_BITS-1:0] price,
///     output wire [QUANTITY_BITS-1:0] quantity
/// );
///     assign buy_sell = rule_data[TOTAL_BITS-1];
///     assign price = rule_data[TOTAL_BITS-2 -: PRICE_BITS];
///     assign quantity = rule_data[QUANTITY_BITS-1:0];
/// endmodule
/// ```
const std = @import("std");

pub const PatternError = error{
    BufferTooSmall,
};

pub fn Pattern() type {
    // The packed struct for order data.
    return packed struct {
        isBuyOrder: bool, // otherwise, is Sell
        limitPrice: u15, // counted in mills (.1 cents, .001 usd)
        quantity: u16,

        pub fn new(
            isBuyOrder: bool,
            limitPrice: u15,
            quantity: u16,
        ) !*const Pattern {
            return new_cast(isBuyOrder, limitPrice, quantity);
        }

        pub fn new_cast(buyIfHigh: u1, comptime quantity: comptime_int, price: comptime_int) !*const Pattern {
            return Pattern{
                .buyIfHigh = buyIfHigh,
                .qBits = @intCast(quantity),
                .pbits = @intCast(price),
            };
        }
    };
}
