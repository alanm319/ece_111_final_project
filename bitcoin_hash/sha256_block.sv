module sha256_block (
 input logic  clk, reset_n, start,
 input logic [31:0] h_init[8],
 input logic [511:0] memory_block,

 output logic[31:0] hash[8],
 output logic done);

parameter integer K[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

parameter integer id = 99;

// FSM state variables
enum logic [2:0] {
    IDLE    = 3'b001,
    BLOCK   = 3'b010,
    COMPUTE = 3'b100
} state;

typedef logic[31:0] logic32;

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[16];
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] a, b, c, d, e, f, g, h;
logic [ 7:0] i;
logic is_done;
// logic [511:0] memory_block;
assign done = is_done;

// logic [511:0] memory_block;

// oh god
assign hash[0] = h0;
assign hash[1] = h1;
assign hash[2] = h2;
assign hash[3] = h3;
assign hash[4] = h4;
assign hash[5] = h5;
assign hash[6] = h6;
assign hash[7] = h7;


// SHA256 hash round
function automatic logic [255:0] sha256_op(input logic [31:0] a, logic32 b, logic32 c,
  logic32 d, logic32 e, logic32 f, logic32 g, logic32 h, logic32 w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    // Student to add remaning code below
    // Refer to SHA256 discussion slides to get logic for this function
    ch = (e & f) ^ ((~e) & g);
    t1 = ch + S1 + h + K[t] + w;
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = maj + S0;
    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction


function automatic logic32 word_expand(input logic[7:0] t);
  logic32 s0, s1;
begin
    if (t < 16) begin
        word_expand = memory_block[512-(t+1)*32 +: 32];
    end
    else begin
        s0 = rightrotate(w[(t-15)%16], 7) ^ rightrotate(w[(t-15)%16], 18) ^ (w[(t-15)%16] >> 3);
        s1 = rightrotate(w[(t-2)%16], 17) ^ rightrotate(w[(t-2)%16],  19) ^ (w[(t-2)%16]  >> 10);
        word_expand = w[(t-16)%16] + s0 + w[(t-7)%16] + s1;
    end
end
endfunction


// rotate `x` right by `r` bits
function automatic logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
   rightrotate = (x >> r) | (x << (32 - r));
endfunction



// SHA-256 FSM
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    state <= IDLE;
  end
  else case (state)
    // Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
    IDLE: begin
        is_done <= 0;
        if(start) begin
        // Student to add rest of the code
            h0 <= h_init[0];
            h1 <= h_init[1];
            h2 <= h_init[2];
            h3 <= h_init[3];
            h4 <= h_init[4];
            h5 <= h_init[5];
            h6 <= h_init[6];
            h7 <= h_init[7];

            a  <= h_init[0];
            b  <= h_init[1];
            c  <= h_init[2];
            d  <= h_init[3];
            e  <= h_init[4];
            f  <= h_init[5];
            g  <= h_init[6];
            h  <= h_init[7];
            // initialize all the counters
            i <= 'b0;

            // prep for transition to READ_WAIT
            // state <= READ_WAIT;
            state <= COMPUTE;
        end
    end

    // For each block compute hash function
    // Go back to BLOCK stage after each block hash computation is completed and if
    // there are still number of message blocks available in memory otherwise
    // move to WRITE stage
    COMPUTE: begin
        if (i < 64) begin
            w[i%16] <= word_expand(i);
            i       <= i + 1;
            {a, b, c, d, e, f, g, h} <= sha256_op(a, b, c, d, e, f, g, h, word_expand(i), i);

        end
        else begin
            h0 <= h0 + a;
            h1 <= h1 + b;
            h2 <= h2 + c;
            h3 <= h3 + d;
            h4 <= h4 + e;
            h5 <= h5 + f;
            h6 <= h6 + g;
            h7 <= h7 + h;
            state  <= IDLE;
            is_done <= 1;
        end

    end

    default: begin
        state <= IDLE;
    end

   endcase
  end

endmodule
