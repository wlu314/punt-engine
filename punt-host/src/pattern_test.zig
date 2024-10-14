const std = @import("std");
const Pattern = @import("../src/pattern.zig").Pattern;
const OrderType = @import("../src/pattern.zig").OrderType;
const PatternError = @import("../src/pattern.zig").PatternError;

test "Pattern encoding and decoding" {
    const orderType1 = OrderType.Buy;
    const quantity1 = 300;
    const limitPrice1 = 600;

    // create pattern
    const pattern1 = try Pattern.new(orderType1, quantity1, limitPrice1);

    // serialize
    const bytes1 = pattern1.toBytes();

    // deserialize back o Pattern
    const deserialized1 = try Pattern.fromBytes(bytes1[0..4]);

    // assert
    try std.testing.expect(deserialized1.getOrderType() == orderType1);
    try std.testing.expect(deserialized1.getQuantity() == quantity1);
    try std.testing.expect(deserialized1.getLimitPrice() == limitPrice1);
}

test "Pattern serialization edge cases" {
    const orderTypeMax = OrderType.Buy;
    const quantityMax = 65535; // max u16
    const limitPriceMax = 32767; // max u15

    const patternMax = try Pattern.new(orderTypeMax, quantityMax, limitPriceMax);

    const bytesMax = patternMax.toBytes();

    const deserializedMax = try Pattern.fromBytes(bytesMax[0..4]);

    try std.testing.expect(deserializedMax.getOrderType() == orderTypeMax);
    try std.testing.expect(deserializedMax.getQuantity() == quantityMax);
    try std.testing.expect(deserializedMax.getLimitPrice() == limitPriceMax);

    const orderTypeMin = OrderType.Sell;
    const quantityMin = 0; // min u16
    const limitPriceMin = 0; // min u15

    const patternMin = try Pattern.new(orderTypeMin, quantityMin, limitPriceMin);

    const bytesMin = patternMin.toBytes();

    const deserializedMin = try Pattern.fromBytes(bytesMin[0..4]);

    try std.testing.expect(deserializedMin.getOrderType() == orderTypeMin);
    try std.testing.expect(deserializedMin.getQuantity() == quantityMin);
    try std.testing.expect(deserializedMin.getLimitPrice() == limitPriceMin);
}

test "Pattern constructor failures" {
    try std.testing.expectError(Pattern.new(OrderType.Buy, 70000, 500)) == PatternError.QuantityTooLarge;

    try std.testing.expectError(Pattern.new(OrderType.Sell, 100, 40000)) == PatternError.PriceTooLarge;

    try std.testing.expectError(Pattern.fromBytes(&[_]u8{ 0xFF, 0xFF })) == PatternError.BufferTooSmall;
}
