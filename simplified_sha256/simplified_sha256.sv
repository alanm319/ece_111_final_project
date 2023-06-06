module simplified_sha256 #(parameter integer NUM_OF_WORDS = 20)(
 input logic  clk, reset_n, start,
 input logic  [15:0] message_addr, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);

localparam Size = NUM_OF_WORDS * 32;
localparam Remainder = Size % 512;
localparam NumPadZeros = 512 - Remainder - 1 - 64;

assign remainder = size % 512;
assign even_fit = remainder == 0;
// FSM state variables
enum logic [2:0] {
  IDLE,
  READ_BLOCK,
  READ_WAIT,
  COMPUTE,
  WRITE,
  WRITE_WAIT
} state;

typedef logic[31:0] logic32;

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[64];
logic [31:0] message[20];
logic [31:0] wt;
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] a, b, c, d, e, f, g, h;
logic [ 7:0] i, j;
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic [512:0] memory_block;
logic [ 7:0] tstep;
logic[7:0] remainder;

logic [5:0] t;
logic32 s0;
logic32 s1;


// SHA256 K constants
// parameter int K[63] = '{
// 32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,
//   32'hab1c5ed5,
// 32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,
//   32'hc19bf174,
// 32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,
//   32'h76f988da,
// 32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,
//   32'h14292967,
// 32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,
//   32'h92722c85,
// 32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,
//   32'h106aa070,
// 32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,
//   32'h682e6ff3,
// 32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,
//   32'hc67178f2
// };

parameter integer PARAM_ARRAY [TOTAL-1 : 0]   = {1, 0, 0, 2}; 
parameter int K[0:63] = {
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


assign num_blocks = determine_num_blocks(NUM_OF_WORDS);
assign tstep = (i - 1);

// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
// Function to determine number of blocks in memory to fetch
function automatic logic [15:0] determine_num_blocks(input logic [31:0] num_words);
  logic[31:0] size;
begin
    // Student to add function implementation
    // TODO: determine if using the division operator here is OK, apparently
    // sometimes it doesn't work
    size = 32 * num_words;

    if (even_fit) begin
      determine_num_blocks = size / 512;
    end
    else begin
      determine_num_blocks = (size / 512) + 1;
    end
end
endfunction


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

function automatic logic32 word_expand(input logic[5:0] t);
  logic32 s0, s1;
begin
    s0 = rightrotate(w[t-15], 7) ^ rightrotate(w[t-15], 18) ^ (w[t-15] >> 3);
    s1 = rightrotate(w[t-2], 17) ^ rightrotate(w[t-2],  19) ^ (w[t-2]  >> 10);
    word_expand = w[t-16] + s0 + w[t-7] + s1;
end
endfunction


// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
// lol no
// assign mem_clk = clk;
// assign mem_addr = cur_addr + offset;
// assign mem_we = cur_we;
// assign mem_write_data = cur_write_data;
/*

module mem_control(
  input logic clk, reset_n, start,
        logic[15:0] base_addr,
        logic[31:0] mem_read_data,
        logic[4:0]  num_words,
        logic should_read,

  output logic done, mem_we,
         logic[15:0] mem_addr,
         logic[31:0] mem_write_data,

  inout logic[511:0] data);

*/

logic[15:0] base_addr;
logic[4:0]  mem_num_words;
logic       mem_done;
logic       mem_should_read;
mem_control ctrl (
    .clk(clk), .reset_n(reset_n), .start(start),
    .base_addr(base_addr),
    .num_words(num_words),
    .should_read(mem_should_read),

    .mem_read_data(mem_read_data),
    .mem_write_data(mem_write_data),
    
    .done(mem_done),
    .mem_we(mem_we),
    .mem_addr(mem_addr),

    .data(memory_block)
);


// Right Rotation Example : right rotate input x by r
// Lets say input x = 1111 ffff 2222 3333 4444 6666 7777 8888
// lets say r = 4
// x >> r  will result in : 0000 1111 ffff 2222 3333 4444 6666 7777
// x << (32-r) will result in : 8888 0000 0000 0000 0000 0000 0000 0000
// final right rotate expression is = (x >> r) | (x << (32-r));
// (0000 1111 ffff 2222 3333 4444 6666 7777) | (8888 0000 0000 0000 0000 0000 0000 0000)
// final value after right rotate = 8888 1111 ffff 2222 3333 4444 6666 7777
// Right rotation function
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
    cur_we <= 1'b0;
    state <= IDLE;
  end
  else case (state)
    // Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
    IDLE: begin
        if(start) begin
        // Student to add rest of the code
            h0 <= 'h6a09e667;
            h1 <= 'hbb67ae85;
            h2 <= 'h3c6ef372;
            h3 <= 'ha54ff53a;
            h4 <= 'h510e527f;
            h5 <= 'h9b05688c;
            h6 <= 'h1f83d9ab;
            h7 <= 'h5be0cd19;

            a  <= 'h6a09e667;
            b  <= 'hbb67ae85;
            c  <= 'h3c6ef372;
            d  <= 'ha54ff53a;
            e  <= 'h510e527f;
            f  <= 'h9b05688c;
            g  <= 'h1f83d9ab;
            h  <= 'h5be0cd19;

            // block counter
            j <= 'b0;

            state <= BLOCK;
        end
    end


    READ: begin
        base_addr <= message_addr;
        num_words <= 16;
        mem_start <= 1;
        state <= READ_WAIT;
    end


    READ_WAIT: begin
        mem_start <= 0;
        if (mem_done) begin
            state <= BLOCK;
        end
    end

    // SHA-256 FSM
    // Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
    // and write back hash value back to memory
    BLOCK: begin
    // Fetch message in 512-bit block size
    // For each of 512-bit block initiate hash value computation
        if (!even_fit && (j + 1) == num_words) begin

        end
        i <= 0;
    end

    // For each block compute hash function
    // Go back to BLOCK stage after each block hash computation is completed and if
    // there are still number of message blocks available in memory otherwise
    // move to WRITE stage
    COMPUTE: begin
    // 64 processing rounds steps for 512-bit block
        if (i <= 64) begin
            if (i < 16) begin
                w[i] <= memory_block[((i+1)*32):(i*32)];
            end
            else begin
                w[i] <= word_expand(i);
            end
            i <= i + 1;
            {a, b, c, d, e, f, g, h} <= sha256_op(a, b, c, d, e, f, g, h, w[i], i);
        end
        else begin
            {h0, h1, h2, h3, h4, h5, h6, h7} <= 
                {h0, h1, h2, h3, h4, h5, h6, h7} + {a, b, c, d, e, f, g, h};
            
            if ((j + 1) < num_blocks) begin
                state <= READ;
                j <= j + 1;
            end
            else begin
                state <= WRITE;
            end
        end

    end

    // h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
    // h0 to h7 after compute stage has final computed hash value
    // write back these h0 to h7 to memory starting from output_addr
    WRITE: begin
        base_addr     <= output_addr;
        mem_num_words <= 8;
        mem_start <= 1;
        state <= WRITE_WAIT;
    end

    WRITE_WAIT: begin
        mem_start <= 0;
        if (mem_done) begin
            state <= IDLE;
        end
    end

    default: begin
        $display("We hit default, this should never happen");
    end

   endcase
  end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule


// When 'start' goes high, this module will either read or write from memory, depending on whether
// `should_read` is high. If it is reading it will read `num_words` words from memory into the
// higher-order bits of `data`. If it is writing it will write `num_words` words from `data[num_words*32:0]`
// into memory, starting at `base_addr` and incrementing.
module mem_control(
  input logic clk, reset_n, start,
        logic[15:0] base_addr,
        logic[31:0] mem_read_data,
        logic[4:0]  num_words,
        logic should_read,

  output logic done, mem_we,
         logic[15:0] mem_addr,
         logic[31:0] mem_write_data,

  inout logic[511:0] data);

typedef enum logic [2:0] {
  IDLE,
  READ_WAIT,
  READ,
  WRITE,
  DONE
} mem_state_t;

mem_state_t state;
// latch whether we should read
logic m_should_read;
logic[4:0] count;

logic[9:0] output_start;
logic[9:0] output_end;
logic[9:0] input_start;
logic[9:0] input_end;

// this offset in count is to account for the fact that memory read output runs one address behind count
assign output_start = 511 - (count - 1) * 32;
assign output_end   = 511 - count * 32;

// don't need offset for writes
assign input_start = (num_words * 32) - count;
assign input_end   = (num_words * 32) - (count + 1);

assign mem_we   = !m_should_read;
assign mem_addr = base_addr + count;

always_ff @(posedge clk, negedge reset_n) begin
    if (!reset_n) begin
        state <= IDLE;
    end

    // idle state handles starting the machine, resetting counter
    case (state)
      IDLE: begin
          if (start == 'b1) begin
              // only need to spin up pipeline if reading, otherwise can go straight to I/O
              if (should_read) begin
                  state <= READ_WAIT;
              end
              else begin
                  state <= WRITE;
              end

              count <= 0;
              done  <= 'b0;
              m_should_read <= should_read;
          end
      end

      // This state allows the pipeline to get spun up for reading, since reads lag one address
      // behind.
      READ_WAIT: begin
          state <= READ;
          count <= 1;
      end


      READ: begin
          if (count <= num_words) begin
              data[output_start:output_end] <= mem_read_data;
          end
          else begin
              state <= DONE;
          end
          count <= count + 1;
      end


      WRITE: begin
          if (count < num_words) begin
              mem_write_data <= data[input_start:input_end];
          end
          else begin
              state <= DONE;
          end
          count <= count + 1;
      end


      DONE: begin
          done  <= 'b1;
          state <= IDLE;
      end

      default: begin
          $display("Hit default in mem_control, this should never happen!");
      end
    endcase
end

endmodule
