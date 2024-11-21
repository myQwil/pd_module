# pd_module

Module for [Pd](https://puredata.info/) externals written in [Zig](https://ziglang.org/).

## How to use it

First, update your `build.zig.zon`:

```
zig fetch --save https://github.com/myQwil/pd_module/archive/refs/tags/v0.1.1.tar.gz
```

Next, add this snippet to your `build.zig` script:

```zig
const pd = b.dependency("pd_module", .{
    .target = target,
    .optimize = optimize,
}).module("pd");
your_external.root_module.addImport("pd", pd),
```

This will import the pd module into `your_external`.
