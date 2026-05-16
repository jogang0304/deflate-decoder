mkdir -p dist
ghc ./main.hs -o dist/decompress

declare -A testcases

# base64 получается командой
# echo -n "Hello" | python3 -c "import sys,zlib,base64; d=sys.stdin.buffer.read(); print(base64.b64encode(zlib.compress(d)[2:-4]).decode())"

testcases=(
    ["S0xKBgA="]="abc"
    ["80jNyckHAA=="]="Hello"
    ["JZDdbQQhDIRbmQq2ieQhkZLrwWG9YAkMwuYu131M7gHJ+Ocbe279Aa4nm0P5ASdzNozK6k+07oUnEtXKJ5oYzycy+4Gvrjkq/JuWMa4eTX3tEe3/WR6Oxg5xgxfRbAduNOdW075ygfFv8FvoINRlMmjygY9eTzxKj0ce0MYxH5ExKybTCcq8sQfeujqJxmZb01wa/VQG09T41SesY/BMLHc+D3y2MdfJmhglCApb1yVJ9gZpVZc7+e575yq5RIgxezggXSEx2+9xBigt5+AbSLdjNig00K+L9dwNJY74fjkVGtRi87TmvoJlGrK8MLqFTLIi8PXlppfwJxJj9DCV2vEH"]="Now eldest new tastes plenty mother called misery get. Longer excuse for county nor except met its things. Narrow enough sex moment desire are. Hold who what come that seen read age its. Contained or estimable earnestly so perceived. Imprudence he in sufficient cultivated. Delighted promotion improving acuteness an newspaper offending he. Misery in am secure theirs giving an. Design on longer thrown oppose am."
)

for key in "${!testcases[@]}"; do
    echo "Testing: $key"
    ./dist/decompress "$key" | diff - <(echo -n "${testcases[$key]}")
    if [ $? -ne 0 ]; then
        echo "Test failed: $key"
        exit 1
    fi
done

echo "All tests passed"
