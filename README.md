# thf

### Preparation:
1. download and untar the Sledgehammer benchmarks, currently hosted at 
http://www.loria.fr/~jablanch/stuff/isa_filter_v2.tgz
2. download and build Lean
3. set `LEAN_PATH` to `<path_to_root_of_this_repo>/sledgehammer_lean:<path_to_lean_repo>/library:.`

### Processing
1. `cabal configure`
2. `cabal build`
3. `python translate_to_lean.py`
4. `cd sledgehammer_lean`
5. `<path_to_lean_repo>/bin/linja`
