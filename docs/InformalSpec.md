# CardStarter Liquidity bridge

The purpose of the CardStarter Liquidity bridge is to provide a two-way gateway for Liquidity providers of SushiSwap and UniDex to automatically move their Liquidity to the SundaeSwap platform and vice-versa. This will require, at minimum, one Contract each on the ethereum platform and the Cardano platform, and some centralized server which approximates the ERC20 token converter from a centralized space.

## General roadmap

1) choose a priority path (Ethereum -> Cardano transactions)
2) build and deploy three components in parallel:
  - Solidity contract (methods for accepting ERC LP tokens)
  - Centralized Conversion Server (methods for watching for confirmations on Ethereum, calling the Cardano contract methods to trigger actions, off-chain data storage as needed.)
  - Cardano Contract (method to receive signed signals from centralized server, token support settings, liquidity reporting)
3) integrate these solutions for a first end-to-end simulator demo
4) Deploy to testnets and attempt a live usage
5) begin work on Secondary priority path (Cardano -> Ethereum path)
6) expand systems in parallel:
  - Solidity (methods to receive signals from centralized server, create liquidity tokens and return them to a specified address)
  - Centralized server (methods to watch for confirmations on the Cardano chain and call methods on them.
  - Cardano contract (method to receive LP tokens.
7) Demo & Deploy
8) finalize testing, Audit prep
9) audit & responses.
10) deployment

  
note:

ERC - ERC20 standard tokens
LP Liquidity Provider

the system will not support or use any tokens on the Cardano network other than Ada

any token bridge action from Cardano to ethereum

## Exceptional token behaviors

the bridge will perform direct exchanges of Eth for Ada, and will utilize a Dex to facilitate this.

In the future, we may need facilities to provide Liquidity for Sundae LP tokens representing the effected Ada/Bridged Eth pair.

### Eth and Ada

if someone coming from the Ethereum network provides tokens from a pair on which includes Eth (such as Eth/BAT), we would mint bridged-Eth, exchange it for Ada, and then set up the beneficiary for the corresponding pair with Ada, (so Ada/bridged-BAT)

if someone from Cardano provides Ada/bridged-BAT, we would first exchange for bridged Eth, then pass those new funds to the Ethereum network in an Eth/BAT pair.
 
## Project Components Specifications

### Ethereum Contract
Languages: Solidity, Testing in Purescript/Typescript

#### AcceptSignedConfig
input: SignedConfiguration

must be from a hardcoded owner address,  
accepts configuration about which tokens are currently supported/available on the Cardano side of the bridge (this will be exclusively controlled by the central server.

#### DepositFromSushiSwap

input: { amount: uint
       , address: CardanoAddress 
       , tokenInfo: TokenPair
       }
-- TokenPair must be sufficient to identify the tokenpair on both networks/dexes (in this case sushiswap and Cardano)

this method will accept Sushiswap LP tokens for the TokenPair (at the specified amount), automatically call sushiswap to exchange them for tokens, and signal the centralized server, keeping the Ethereum-side funds as liquidity for exchanges from Cardano back to Ethereum

#### DepositFromUniSwap

input: { amount: uint
       , address: CardanoAddress 
       , tokenInfo: TokenPair
       }
-- TokenPair must be sufficient to identify the tokenpair on both networks/dexes (in this case sushiswap and Cardano)

This method will accept Uniswap LP tokens for the TokenPair (at the specified amount), automatically call uniswap to exchange them for tokens, and signal the centralized server, keeping the Ethereum-side funds as liquidity for exchanges from Cardano back to Ethereum

#### AcceptSignedPaymentConfirmationToSushiSwap
input: { address: EthAddress
       , amount: Map Token uint
       , signature: Signature
       , tokenInfo: TokenPair
       }
       
This method will receive a signed message from the Centralized server when a successful deposit on the cardano network is confirmed.

attempts to deposit the tokens as liquidity at sushiswap at the nearest possible ratio to the ratio received, with the resulting LP tokens and and remainder from the payment tokens transferred to the specified address.


#### AcceptSignedPaymentConfirmationToUniswap
input: { address: EthAddress
       , amount: Map Token uint
       , signature: Signature
       , tokenInfo: TokenPair
       }
       
This method will receive a signed message from the Centralized server when a successful deposit on the cardano network is confirmed.

attempts to deposit the tokens as liquidity at uniswap at the nearest possible ratio to the ratio received, with the resulting LP tokens and and remainder from the payment tokens transferred to the specified address.

### Centralized Server
Languages: Haskell (Servant)
 each bridge UI may need to pass a subscription token to the server by http.
 
this server will keep a websocket subscription monitoring the ethereum network and cardano networks for transactions against each  side of the bridge, passing signals to  the opposite side in order 

if possible, this server may make use of the ERC20 token converter project, or similar technology.

note: if webhook subscriptions are unavailable, we may need to use polling here.

if database connections are required, we would look to redis, or postgresql.


#### SushiSwapToSundae webhook subscription
after sufficient confirmations, Cardano Contract's AcceptSignedPaymentConfirmationToSundae is called.

#### UniSwapToSundae webhook subscription
after sufficient confirmations,  Cardano Contract's AcceptSignedPaymentConfirmationToSundae is called.

#### SundaeSwapToSushiSwap subscription
after sufficient confirmations, Ethereum Contract's AcceptSignedPaymentConfirmationToSushiSwap is called

#### SundaeSwapToUniswap subscription
after sufficient confirmations, Ethereum Contract's AcceptSignedPaymentConfirmationToUniswap is called


### Cardano Contract
Languages: Haskell (Plutus)


#### DepositToUniswap

input: { amount: Integer
       , address: EthAddress
       , tokenInfo: TokenPair
       }
-- TokenPair must be sufficient to identify the tokenpair on both networks/dexes (in this case sushiswap and Cardano)

this method will accept Sundaeswap LP tokens for the TokenPair (at the specified amount), automatically call sushiswap to exchange them for tokens, and signal the centralized server, keeping the Ethereum-side funds as liquidity for exchanges from Cardano back to Ethereum

#### DepositToSushiSwap

input: { amount: Integer
       , address: EthAddress
       , tokenInfo: TokenPair
       }
-- TokenPair must be sufficient to identify the tokenpair on both networks/dexes (in this case sushiswap and Cardano)

this method will accept Sundaeswap LP tokens for the TokenPair (at the specified amount), automatically call sushiswap to exchange them for tokens, and signal the centralized server, keeping the Ethereum-side funds as liquidity for exchanges from Cardano back to Ethereum

#### AcceptSignedPaymentConfirmationToSundae
input: { address: CardanoAddress
       , amount: PlutusTx.Value
       , signature: Signature
       , tokenInfo: TokenPair
       }

This method will receive a signed message from the Centralized server when a successful deposit on the cardano network is confirmed.  
Diagram of transaction can be found [here](../docs/eutxo-design/PaymentConfirmationSundae.png)

attempts to deposit the tokens as liquidity at SundaeSwap at the nearest possible ratio to the ratio received, with the resulting LP tokens and and remainder from the payment tokens transferred to the specified address.



