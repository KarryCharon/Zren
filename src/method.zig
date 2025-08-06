const VM = @import("vm.zig");
const V = @import("value.zig");
const ZrenVM = VM.ZrenVM;
const ZrenForeignMethodFn = VM.ZrenForeignMethodFn;

const Value = V.Value;
const ObjClosure = V.ObjClosure;

const GenericBuffer = @import("buffer.zig").GenericBuffer;
pub const MethodBuffer = GenericBuffer(Method);

const Primitive = *const fn (vm: *ZrenVM, args: []Value) bool;

pub const Method = struct {
    pub const MethodType = enum {
        METHOD_PRIMITIVE,
        METHOD_FUNCTION_CALL,
        METHOD_FOREIGN,
        METHOD_BLOCK,
        METHOD_NONE,
    };
    method_type: MethodType = .METHOD_NONE,
    as: union {
        primitive: Primitive,
        foreign: ?ZrenForeignMethodFn,
        closure: *ObjClosure,
    } = undefined,
};
