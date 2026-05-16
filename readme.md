## Компиляция программы
`ghc ./main.hs -o decompress`

## Получение сжатой строки
`echo -n "Hello" | python3 -c "import sys,zlib,base64; d=sys.stdin.buffer.read(); print(base64.b64encode(zlib.compress(d)[2:-4]).decode())"`

Вместо Hello надо подставить свою строку.

## Запуск
`./decompress <base64>`

`<base64>` получается из предыдущего пункта.
