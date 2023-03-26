#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

echo -e -n "Running tests...\n"

#Test "-i" (curve print)
echo -e -n "\tTest \"-i\" curve print\t\t\t\t\t"
cat tests/curve > tests/test1.in
cat tests/curve > tests/test1.out
./flp22-fun -i tests/test1.in > tests/test1.real
if [[ $(diff tests/test1.out tests/test1.real) = "" ]]
then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
fi

#generate key
cat tests/curve > tests/test2.in
./flp22-fun -k tests/test2.in > tests/key_self

#signature with self generated key
cat tests/curve > tests/test3.in
cat tests/hash >> tests/test3.in
cat tests/key_self >> tests/test3.in
./flp22-fun -s tests/test3.in > tests/sign_self

#valid signature verification
echo -e -n "\tGenerate Key -> Sign -> Verify valid signature\t\t"
cat tests/curve > tests/test4.in
cat tests/hash >> tests/test4.in
cat tests/key_self >> tests/test4.in
cat tests/sign_self >> tests/test4.in
echo True > tests/test4.out
./flp22-fun -v tests/test4.in > tests/test4.real
if [[ $(diff tests/test4.out tests/test4.real) = "" ]]
then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
fi

#invalid signature verification
echo -e -n "\tGenerate Key -> Sign -> Spot invalid signature\t\t"
cat tests/curve > tests/test5.in
cat tests/hash >> tests/test5.in
cat tests/key_self >> tests/test5.in
cat tests/sign_pre >> tests/test5.in
echo False > tests/test5.out
./flp22-fun -v tests/test5.in > tests/test5.real
if [[ $(diff tests/test5.out tests/test5.real) = "" ]]
then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
fi

#signature with provided generated key

#valid signature verification
echo -e -n "\tLoad Key and Signature -> Verify valid signature\t"
cat tests/curve > tests/test6.in
cat tests/hash >> tests/test6.in
cat tests/key_pre >> tests/test6.in
cat tests/sign_pre >> tests/test6.in
echo True > tests/test6.out
./flp22-fun -v tests/test6.in > tests/test6.real
if [[ $(diff tests/test6.out tests/test6.real) = "" ]]
then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
fi

#invalid signature verification
echo -e -n "\tLoad Key and Signature -> Spot invalid signature\t"
cat tests/curve > tests/test7.in
cat tests/hash >> tests/test7.in
cat tests/key_pre >> tests/test7.in
cat tests/sign_self >> tests/test7.in
echo False > tests/test7.out
./flp22-fun -v tests/test7.in > tests/test7.real
if [[ $(diff tests/test7.out tests/test7.real) = "" ]]
then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
fi

echo -e -n "Testing finished\n"