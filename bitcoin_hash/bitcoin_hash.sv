module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter int NUM_NONCES = 16;
parameter int NUM_BLOCKS = 8;
// 19 words of message, 1 nonce
parameter int NUM_OF_WORDS = 19;

logic [31:0] hout[NUM_NONCES];

logic[511:0] memory_input[NUM_BLOCKS];
logic block_start;
logic[NUM_BLOCKS-1:0] block_done;

logic[31:0] init_hash[8];
logic[31:0] init_alpha[8];

logic[31:0] end_hash[NUM_BLOCKS][8];
logic[31:0] end_alpha[NUM_BLOCKS][8];

localparam [63:0] MessageSize = (NUM_OF_WORDS + 1) * 32;
logic [31:0] message[NUM_OF_WORDS];
logic [15:0] offset;

logic [31:0] cur_write_data;
logic [15:0] cur_addr;

logic random_flag;

genvar i;
generate
for (i = 0; i < NUM_BLOCKS; i++) begin: sha_blocks
    sha256_block block(
        .clk(clk), .reset_n(reset_n),
        .start(block_start),
        .memory_block(memory_input[i]),

        .h_init(init_hash),
        .alpha_init(init_alpha),

        .hash(end_hash[i]),
        .alpha(end_alpha[i]),
        .done(block_done[i]));
    defparam block.id = i;
end
endgenerate

parameter int K[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

parameter logic[31:0] HASH_CONSTANTS[8] = '{
    32'h6a09e667,
    32'hbb67ae85,
    32'h3c6ef372,
    32'ha54ff53a,
    32'h510e527f,
    32'h9b05688c,
    32'h1f83d9ab,
    32'h5be0cd19
};

// Student to add rest of the code here
enum logic[4:0] {
    IDLE,
    READ,
    PHASE1,
    PHASE2_1,
    PHASE2_2,
    PHASE3_1,
    PHASE3_2,
    WRITE
} state;

// enable writing in WRITE
assign mem_we = (state == WRITE);
assign mem_addr = cur_addr + offset;
assign mem_clk = clk;
assign mem_write_data = cur_write_data;

always_ff @( posedge clk, negedge reset_n) begin
    if (!reset_n) begin
        state <= IDLE;
    end
    else case (state)
        IDLE: begin
            random_flag <= 0;
            if (start) begin
                state <= READ;
                offset <= 0;
                cur_addr <= message_addr;
            end
        end

        READ: begin
            if (offset <= NUM_OF_WORDS) begin
                if (offset > 0) begin
                    $displayh("reading data %p", mem_read_data);
                    message[offset-1] <= mem_read_data;
                end
                offset <= offset + 1;
            end
            else begin
                init_hash <= HASH_CONSTANTS;
                init_alpha <= HASH_CONSTANTS;

                for (int i = 0; i < 16; i++) begin
                    memory_input[0][(i*32) +: 32] <= message[i];
                end
                state <= PHASE1;
                block_start <= 1;
                random_flag <= 1;
            end
        end

        PHASE1: begin
            if (block_done[0]) begin
                $displayh("Output of phase1 is %p", end_hash[0]);
                for (logic[31:0] i = 0; i < NUM_BLOCKS; i++) begin
                        init_hash <= end_hash[0];
                        init_alpha <= end_alpha[0];
                        // memory_input[i] <= {
                        //     // i do not know why we need to multiply size by 2
                        //     // but otherwise it doesn't match the testbench padding
                        //     MessageSize*2,
                        //     {9{32'b0}},
                        //     32'h80000000,
                        //     i, // nonce
                        //     96'b0 // (message[16:18])
                        // };
                        memory_input[i][63:0] <= MessageSize;
                        memory_input[i][351:64] <= 288'b0;
                        memory_input[i][383:352] <= 32'h80000000;
                        memory_input[i][415:384] <= i;

                        for (int j = 0; j < 3; j++) begin
                            memory_input[i][(512-32*(j+1)) +: 32] <= message[(16+j)];
                        end

                        block_start <= 1;
                end
                // state <= PHASE2_1;
                state <= PHASE2_1;
                random_flag <= 1;
            end
            else begin
                // keep block_start from immediately starting the 0th block on completion
                block_start <= 0;
            end
        end

        PHASE2_1: begin
            if (random_flag) begin
                random_flag <= 0;

                for (int i = 0; i < NUM_BLOCKS; i++) begin
                    $displayh("memory input to all phase2 nonce %d is %p", i, memory_input[i]);
                end
            end
            if (& block_done) begin
                // setup for phase 3
                for (int i = 0; i < NUM_BLOCKS; i++) begin
                    $displayh("output of phase2 block %d is %p", i, end_hash[i]);
                    init_hash <= HASH_CONSTANTS;
                    init_alpha <= HASH_CONSTANTS;
                    memory_input[i] <= {
                        64'd512,
                        {5{32'b0}},
                        32'h80000000,
                        256'b0 // (end_hash[i])
                    };
                    for (int j = 0; j < 8; j++) begin
                        memory_input[i][(32*j+256) +: 32] <= end_hash[i][j];
                    end
                end
                random_flag <= 0;
                state <= PHASE3_1;
                block_start <= 1;
            end
            else begin
                block_start <= 0;
            end
        end

        PHASE2_2: begin
            $displayh("unimplemented");
        end

        PHASE3_1: begin
            if (!random_flag) begin
                random_flag <= 1;
                $displayh("memory input to all phase3 blocks is %p", memory_input);
            end
            if (& block_done) begin
                $displayh("phase3 outputs: %p", end_hash);
                offset <= 0;
                state <= WRITE;
                cur_addr <= output_addr;

                // set up data for beginning of write
                cur_write_data <= end_hash[0][0];
            end
        end

        PHASE3_2: begin
            $displayh("unimplemented");
        end

        WRITE: begin
            // copy zero'th block of final hash for each nonce to memory
            if (offset < 15) begin
                offset <= offset + 1;
                cur_write_data <= end_hash[offset+1][0];
            end 
            else begin
                state <= IDLE;
            end
        end

        default: begin
            $displayh("Hit default in bitcoin_hash");
        end
    endcase
end


assign done = (state == IDLE);


endmodule
