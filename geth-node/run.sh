geth \
    --datadir=./chaindata \
    --networkid=12314 \
    --http \
    --http.api=web3,eth,debug,personal,net \
    --http.corsdomain="package://6fd22d6fe5549ad4c4d8fd3ca0b7816b.mod" \
    --ws \
    --ws.origins="localhost" \
    --allow-insecure-unlock \
    --unlock=0xE3C0e3A942fE518A9cAc0185d9AcffafCc4Fdce2 \
    --password=keydata/password \
    --mine \
    --miner.threads=1 \
    console