const std = @import("std");

const Opcode = enum {
    acc,
    jmp,
    nop,
};

const Instruction = struct {
    opcode: Opcode,
    imm: i64,
};

const RunResult = struct {
    last_instruction: usize,
    acc: i64,
};

fn runProgram(visited: []bool, program: []const Instruction) RunResult {
    std.mem.set(bool, visited, false);
    var acc: i64 = 0;

    var ip: usize = 0;
    var ip_delta: i64 = 0;
    while (ip < program.len and !visited[ip]) : (ip = @intCast(usize, @intCast(i64, ip) + ip_delta)) {
        ip_delta = 1;
        const instr = program[ip];
        switch (instr.opcode) {
            .acc => acc += instr.imm,
            .jmp => ip_delta = instr.imm,
            .nop => {},
        }
        visited[ip] = true;
    }
    return RunResult{
        .last_instruction = ip,
        .acc = acc,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = &gpa.allocator;
    defer _ = gpa.deinit();

    var input = try std.fs.cwd().readFileAlloc(allocator, "08.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var program = std.ArrayList(Instruction).init(allocator);
    defer program.deinit();

    var it = std.mem.tokenize(input, "\n");
    while (it.next()) |line| {
        var word_it = std.mem.tokenize(line, " ");
        const opcode = switch (word_it.next().?[0]) {
            'a' => Opcode.acc,
            'j' => Opcode.jmp,
            'n' => Opcode.nop,
            else => unreachable,
        };
        const imm = std.fmt.parseInt(i64, word_it.next().?, 10) catch unreachable;
        try program.append(Instruction{
            .opcode = opcode,
            .imm = imm,
        });
    }

    var visited = try allocator.alloc(bool, program.items.len);
    defer allocator.free(visited);

    const original_res = runProgram(visited, program.items);
    std.debug.print("EASY: {}\n", .{original_res.acc});

    for (program.items) |*instr, idx| {
        const fixed_opcode = switch (instr.opcode) {
            .acc => Opcode.acc,
            .nop => Opcode.jmp,
            .jmp => Opcode.nop,
        };
        if (fixed_opcode != instr.opcode) {
            const original_opcode = instr.opcode;
            instr.opcode = fixed_opcode;
            const res = runProgram(visited, program.items);
            if (res.last_instruction >= program.items.len) {
                std.debug.print("HARD: {}\n", .{res.acc});
                break;
            }
            instr.opcode = original_opcode;
        }
    }
}
