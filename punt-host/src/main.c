#include "../include/pattern.h"
#include "../include/log.h"

// Function prototypes from dma.c
int fpga_init();
void fpga_cleanup();
int send_pattern(pattern_t pattern, uint32_t offset);

int main() {
    log_init();
    log_message(LOG_LEVEL_INFO, "Starting DMA application");

    if (fpga_init() != 0) {
        log_message(LOG_LEVEL_ERROR, "Failed to initialize FPGA DMA interface");
        return 1;
    }

    OrderType order_type = BUY;       // BUY or SELL
    uint16_t price = 12345;           // Price in mills (e.g., 12.345 units)
    uint16_t quantity = 1000;         // Quantity

    pattern_t pattern = pack_pattern(order_type, price, quantity);
    log_message(LOG_LEVEL_INFO, "Packed pattern: OrderType=%s, Price=%u, Quantity=%u",
                order_type == BUY ? "BUY" : "SELL", price, quantity);

    if (send_pattern(pattern, 0) != 0) {
        log_message(LOG_LEVEL_ERROR, "Failed to send pattern to FPGA");
        fpga_cleanup();
        log_cleanup();
        return 1;
    }

    log_message(LOG_LEVEL_INFO, "Pattern sent successfully");

    fpga_cleanup();
    log_cleanup();
    return 0;
}

