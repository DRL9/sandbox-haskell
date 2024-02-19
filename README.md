# sandbox-haskell


# 工具
- GHCup: haskell 安装器, 用来管理工具链 `ghcup tui`
- GHC: 编译器
- HLS: LSP
## 构建工具
两个都安装（因为不同的项目用的不一样）。 自己使用 stack
- Cabal `cabal`
- Stack `stack`

## GHCI
- `ghci`
- repl
- 加载文件 `:l filepath`
- 重新加载所有文件 `:r`
- 加载内置模块 `:module modName`
- 最后一个表达式的值会储存在 `it` 变量
- `:set +t` 可以每次都输出表达式结果的类型
- `:unset +t` 取消
- `:type var` 或 `:t var` 查看类型

## ghc-pkg
- 包管理工具
- `ghc-pkg list` 列出安装的包


## stack
- `stack setup` 安装 GHC 工具链
- `stack repl` 进入 ghci
- 运行 `stack build` ,  会根据 `package.yml` 生成对应的 `*.cabal`

# 目录结构
- `.cabal` 库的描述信息
- `Setup.hs` 用来定制构建过程