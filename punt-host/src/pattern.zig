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

pub const OrderType = enum(u1) {
    Sell = 0,
    Buy = 1,
};

pub const PatternError = error{
    QuantityTooLarge,
    PriceTooLarge,
    BufferTooSmall,
    InvalidOrderType,
};

pub const Pattern = packed struct {
    limitPrice: u15, // counted in mills (.1 cents, .001 usd)
    isBuyOrder: OrderType, // otherwise, is Sell
    quantity: u16,

    pub fn new(isBuyOrder: OrderType, limitPrice: u15, quantity: u16) !Pattern {
        return Pattern{
            .limitPrice = @intCast(limitPrice),
            .isBuyOrder = isBuyOrder,
            .quantity = @intCast(quantity),
        };
    }

    pub fn new_unchecked_cast(comptime limitPrice: comptime_int, orderType: OrderType, quantity: comptime_int) !*const Pattern {
        return Pattern{
            .limitPrice = @intCast(limitPrice),
            .orderType = orderType,
            .quantity = @intCast(quantity),
        };
    }

    pub fn getOrderType(self: Pattern) OrderType {
        return self.isBuyOrder;
    }

    pub fn getQuantity(self: Pattern) u16 {
        return self.quantity;
    }

    pub fn getLimitPrice(self: Pattern) u16 {
        return @intCast(self.limitPrice);
    }

    /// serialize into a 4-byte array
    pub fn toBytes(self: Pattern) [4]u8 {
        return @bitCast(self);
    }

    /// deserialize 4-byte array into a Pattern instance
    pub fn fromBytes(bytes: []const u8) !Pattern {
        if (bytes.len < 4) {
            return PatternError.BufferTooSmall;
        }

        const received_bytes: [4]u8 = .{
            bytes[0],
            bytes[1],
            bytes[2],
            bytes[3],
        };

        const u32_val: u32 = @bitCast(received_bytes);
        const pattern: Pattern = @bitCast(u32_val);

        // Validate enum
        if (pattern.isBuyOrder != OrderType.Sell and pattern.isBuyOrder != OrderType.Buy) {
            return PatternError.InvalidOrderType;
        }

        return pattern;
    }
};
