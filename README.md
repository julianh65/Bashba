# Bashba

### Scanning

```
ocamlbuild scantest.native
./scantest.native
```

### Parsing

```
ocamlbuild semanttest.native
./semanttest.native
```

### IRGen

Install llvm

```
opam install llvm
```

Build the compiler

```
f
export PATH=/usr/local/opt/llvm/bin:$PATH
```

Run code

```
./bashba.native -l yourcode.mc > a.out
lli a.out
```
