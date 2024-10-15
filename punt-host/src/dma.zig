const std = @import("std");

pub const DmaError = error{
    DMATransferFailed,
};

extern "c" fn ioctl(fd: std.posix.fd_t, request: c_ulong, argp: anyopaque) c_int;

// placeholder, this will need to match the XDMA driver
const FPGA_DMA_WRITE: c_ulong = 0x40046F00;

pub fn sendDMA(fd: std.posix.fd_t, buffer: []const u8) !void {
    const ret = ioctl(fd, FPGA_DMA_WRITE, buffer.ptr);
    if (ret < 0) {
        return DmaError.DMATransferFailed;
    }
}
