const std = @import("std");
const Pattern = @import("pattern.zig").Pattern;
const OrderType = @import("pattern.zig").OrderType;
const PatternError = @import("pattern.zig").PatternError;
const sendDMA = @import("dma.zig").sendDMA;
const DMAError = @import("dma.zig").DmaError;

pub fn main() !void {
    var allocator = std.heap.page_allocator;
    const device_path = "/dev/xdma0_h2c_0"; // host-to-card dma channel

    const file = try std.fs.cwd().openFile(device_path, .{});
    defer file.close();

    const num_patterns = 2;

    var patterns = try allocator.alloc(Pattern, num_patterns);
    defer allocator.free(patterns);

    patterns[0] = try Pattern.new(OrderType.Buy, 1, 1_100);
    patterns[1] = try Pattern.new(OrderType.Sell, 1, 1_200);

    const bytes_per_pattern = 4; // 32 bits = 4 bytes
    const total_bytes = num_patterns * bytes_per_pattern;

    var serialized_buffer = try allocator.alloc(u8, total_bytes);
    defer allocator.free(serialized_buffer);

    // serialize each Pattern into the buffer
    for (patterns, 0..) |pattern, i| {
        const bytes = pattern.toBytes();
        @memcpy(serialized_buffer[i * bytes_per_pattern .. (i + 1) * bytes_per_pattern], bytes[0..4]);
    }

    // allocate DMA buffer with proper alignment
    const buffer_align = 8; // 8-byte alignment for DMA
    const dma_buffer = try allocator.alignedAlloc(u8, buffer_align, total_bytes);
    defer allocator.alignedFree(dma_buffer);

    // copy serialized data into DMA buffer
    @memcpy(dma_buffer, serialized_buffer);

    // make DMA transfer using ioctl
    try sendDMA(file, dma_buffer) catch |err| {
        std.debug.print("DMA transfer failed: {}\n", .{err});
        return;
    };

    std.debug.print("DMA transfer of {} Patterns completed successfully.\n", .{num_patterns});
}
