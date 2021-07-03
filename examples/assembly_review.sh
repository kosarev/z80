set -e
clang++ -I.. -DNDEBUG -O3 -c -S \
    -fstrict-aliasing -fverbose-asm \
    -o assembly_review.t assembly_review.cpp
expand assembly_review.t >assembly_review.s
rm assembly_review.t
