# 简介
1. 复现自编程语言 [wren](https://wren.io)
2. 原wren中所有测试用例均测试通过
3. 原wren中所有benchmark用例均正常运行
4. 使用zig实现

# 测试
## 运行脚本
1. `zig build`
2. `./zig-out/bin/Zren some_script.wren`
3. 测试脚本可以在 https://github.com/wren-lang/wren/tree/main/example 中找到

# 后续计划
1. variable.index  改为usize
2. 检查所有 -1 的变量
3. 检查所有的len 相关变量
4. 检查所有TODO
5. gc流程
6. hash码改进
7.  编译改进为状态机
8.  内存分配改为默认使用 realloc
9.  gc中传递user_data (alloc realloc等)
10. 重构userdata 传递
11. 删除字符串用 \x00判断结束(zig的slice自带长度信息)
12. profiling