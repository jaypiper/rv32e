module UpdateInst (
    input           clock,
    input [63:0]    pc,
    input [31:0]    inst,
    input           valid
);

import "DPI-C" function void update_pc(int valid, int pc, int inst);

always @(posedge clock) begin
    update_pc(valid, pc, inst);
end

endmodule