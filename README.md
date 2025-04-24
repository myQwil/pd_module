# pd_module

Module for [Pd](https://puredata.info/) externals written in [Zig](https://ziglang.org/).

## How to use it

First, update your `build.zig.zon`:

```
zig fetch --save git+https://github.com/myQwil/pd_module#v0.2.4
```

Next, add this snippet to your `build.zig` script:

```zig
const pd_dep = b.dependency("pd_module", .{
    .target = target,
    .optimize = optimize,
});
your_external.root_module.addImport("pd", pd_dep.module("pd")),
```

This will import the pd module into `your_external`.
