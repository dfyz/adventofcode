#include <cstdint>
#include <exception>
#include <fstream>
#include <iostream>
#include <limits>
#include <memory>
#include <stack>
#include <string>
#include <vector>
#include <utility>

using TNum = uint16_t;
static constexpr size_t REGISTER_COUNT = 8;
using TRegisters = TNum[REGISTER_COUNT];
using TStack = std::vector<TNum>;
using TData = TNum*;

class TTracer {
public:
    TTracer(const char* traceFile)
        : Writer(traceFile)
    {}

    template <typename T>
    TTracer& operator<<(const T& value) {
        if (EnableTracing) {
            Writer << value;
        }
        return *this;
    }

    void SetTracing(bool value) {
        EnableTracing = value;
    }

private:
    std::ofstream Writer;
    bool EnableTracing = false;
};

class TVm {
public:
    using TCommand = std::pair<void (TVm::*)(), const char*>;

    TVm(TData data, size_t dataLength, bool tracingMode = false, size_t startIp = 0)
        : Data(data)
        , DataLength(dataLength)
        , Ip(startIp)
        , Tracer("memory.dump")
        , OutInterceptor(*this)
    {}

    void Halt() {
        Ip = DataLength;
    }

    void Set() {
        auto& reg = GetRegisterReference();
        auto value = LoadAndInterpretNum();
        reg = value;
    }

    void Push() {
        auto value = LoadAndInterpretNum();
        Stack.push_back(value);
    }

    void Pop() {
        auto& reg = GetRegisterReference();
        EnsureNonEmptyStack();
        reg = Stack.back();
        Stack.pop_back();
    }

    void Eq() {
        auto& reg = GetRegisterReference();
        auto a = LoadAndInterpretNum();
        auto b = LoadAndInterpretNum();
        reg = a == b;
    }

    void Gt() {
        auto& reg = GetRegisterReference();
        auto a = LoadAndInterpretNum();
        auto b = LoadAndInterpretNum();
        reg = a > b;
    }

    void Jmp() {
        auto target = LoadAndInterpretNum();
        Ip = target;
    }

    void Jt() {
        auto val = LoadAndInterpretNum();
        auto target = LoadAndInterpretNum();
        if (val != 0) {
            Ip = target;
        }
    }

    void Jf() {
        auto val = LoadAndInterpretNum();
        auto target = LoadAndInterpretNum();
        if (val == 0) {
            Ip = target;
        }
    }

    void Add() {
        auto& reg = GetRegisterReference();
        auto a = LoadAndInterpretNum();
        auto b = LoadAndInterpretNum();
        reg = Clamp(a + b);
    }

    void Mul() {
        auto& reg = GetRegisterReference();
        auto a = LoadAndInterpretNum();
        auto b = LoadAndInterpretNum();
        auto prod = static_cast<uint32_t>(a) * b;
        reg = Clamp(a * b);
    }

    void Mod() {
        auto& reg = GetRegisterReference();
        auto a = LoadAndInterpretNum();
        auto b = LoadAndInterpretNum();
        reg = a % b;
    }

    void And() {
        auto& reg = GetRegisterReference();
        auto a = LoadAndInterpretNum();
        auto b = LoadAndInterpretNum();
        reg = a & b;
    }

    void Or() {
        auto& reg = GetRegisterReference();
        auto a = LoadAndInterpretNum();
        auto b = LoadAndInterpretNum();
        reg = a | b;
    }

    void Not() {
        auto& reg = GetRegisterReference();
        auto a = LoadAndInterpretNum();
        reg = Clamp(~a);
    }

    void Rmem() {
        auto& reg = GetRegisterReference();
        auto addr = LoadAndInterpretNum();
        reg = Data[addr];
    }

    void Wmem() {
        auto addr = LoadAndInterpretNum();
        auto value = LoadAndInterpretNum();
        Data[addr] = value;
    }

    void Call() {
        auto target = LoadAndInterpretNum();
        if (target == 6027) {
            Ip = 5498;
            return;
        }
        Stack.push_back(static_cast<TNum>(Ip));
        Ip = target;
    }

    void Ret() {
        EnsureNonEmptyStack();
        auto addr = Stack.back();
        Stack.pop_back();
        Ip = addr;
    }

    void Out() {
        auto ch = static_cast<char>(LoadAndInterpretNum());
        Tracer << " ('" << ch << "')";
        OutInterceptor.Intercept(ch);
    }

    void In() {
        auto& reg = GetRegisterReference();
        int ch = std::cin.get();
        reg = ch;
    }

    void Noop() {
    }

    void Run() {
        const std::vector<TCommand> Commands = {
            { &TVm::Halt, "halt" },
            { &TVm::Set, "set" },
            { &TVm::Push, "push"},
            { &TVm::Pop, "pop" },
            { &TVm::Eq, "eq" },
            { &TVm::Gt, "gt" },
            { &TVm::Jmp, "jmp" },
            { &TVm::Jt, "jt" },
            { &TVm::Jf, "jf" },
            { &TVm::Add, "add" },
            { &TVm::Mul, "mul" },
            { &TVm::Mod, "mod" },
            { &TVm::And, "and" },
            { &TVm::Or, "or" },
            { &TVm::Not, "not" },
            { &TVm::Rmem, "rmem" },
            { &TVm::Wmem, "wmem" },
            { &TVm::Call, "call" },
            { &TVm::Ret, "ret" },
            { &TVm::Out, "out" },
            { &TVm::In, "in" },
            { &TVm::Noop, "noop" },
        };

        while (Ip < DataLength) {
            auto opCode = Data[Ip];
            if (opCode >= Commands.size()) {
                Tracer << "Unknown opcode at " << Ip << ": " << opCode << "; terminating\n";
                break;
            }

            auto cmd = Commands[opCode];
            Tracer << Ip << ": " << cmd.second;
            ++Ip;
            ((this)->*(cmd.first))();
            DumpStorage();
            Tracer << "\n";
        }
    }

private:
    class TOutputInterceptor {
    public:
        TOutputInterceptor(TVm& vm)
            : Vm(vm)
        {}

        void Intercept(char ch) {
            if (ch == '\n') {
                Vm.Tracer << "\n!STRING: " << Line << "\n";
                std::cout << Line << std::endl;
                Line.clear();
            } else {
                Line += ch;
            }
        }

    private:
        TVm& Vm;
        std::string Line;
    };

    void DumpStorage() {
        Tracer << ", REGISTERS: [";
        for (size_t i = 0; i < REGISTER_COUNT; ++i) {
            Tracer << (i > 0 ? " " : "") << Regs[i];
        }
        Tracer << "], STACK: [";
        for (size_t i = 0; i < Stack.size(); ++i) {
            Tracer << (i > 0 ? " " : "") << Stack[i];
        }
        Tracer << "]";
    }

    void EnsureNonEmptyStack() const {
        if (Stack.empty()) {
            throw std::runtime_error("Empty stack");
        }
    }

    TNum Clamp(TNum num) const {
        return num & 0b0111111111111111;
    }

    TNum LoadNum() {
        return Data[Ip++];
    }

    TNum LoadAndInterpretNum() {
        auto num = LoadNum();
        if (num <= 32767) {
            Tracer << " " << num;
            return num;
        } else {
            return GetRegisterReference(num);
        }
    }

    TNum& GetRegisterReference(TNum num) {
        static constexpr TNum offset = 32768;
        if (num < offset || num >= offset + REGISTER_COUNT) {
            throw std::runtime_error("Invalid register reference: " + std::to_string(num));
        }
        auto regIdx = num - offset;
        if (regIdx == 7 && Ip > 600) {
            Regs[regIdx] = 25734;
        }
        Tracer << " regs[" << regIdx << "]";
        return Regs[regIdx];
    }

    TNum& GetRegisterReference() {
        return GetRegisterReference(LoadNum());
    }

    TData Data;
    size_t DataLength;
    TRegisters Regs;
    TStack Stack;
    size_t Ip = 0;
    TTracer Tracer;
    TOutputInterceptor OutInterceptor;
};

size_t GetStreamLength(std::ifstream& reader) {
    reader.seekg(0, reader.end);
    auto result = reader.tellg();
    reader.seekg(0, reader.beg);
    return result;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cerr << "Usage: BYTECODE_FILE\n";
        return 1;
    }

    std::string bytecodeFile(argv[1]);
    std::ifstream reader(bytecodeFile, std::ifstream::binary);
    if (!reader) {
        throw std::runtime_error("Failed to open " + bytecodeFile);
    }

    auto length = GetStreamLength(reader);
    std::vector<char> bytes(length);
    reader.read(bytes.data(), length);

    TVm vm(reinterpret_cast<TData>(bytes.data()), length);
    vm.Run();

    return 0;
}