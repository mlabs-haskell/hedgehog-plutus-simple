// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.7.6;
pragma abicoder v2;

import "IERC20.sol";
import "IUniswapV2Pair.sol";

/**
Liquidity Bridge deposit contract
Ethereum UniSwapv2 factory address: 0x5C69bEe701ef814a2B6a3EDD4B1652CB9cc5aA6f
*/
contract LiquidityBridgeDeposit {
  address factory;
  bytes32[] cardanoDexes;
  address owner;
  IERC20 weth = IERC20(0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2);

  // Event thrown when a successful deposit occurs, including liquidity withdrawl
  event Deposit(
    address sender,
    bytes28 cardanoAddress,
    bytes32 dexName,
    string token0,
    string token1,
    address addr0,
    address addr1,
    uint256 amount0,
    uint256 amount1
  );

  // Event for Eth/WEth deposits
  event DepositEth(
    address sender,
    bytes28 cardanoAddress,
    uint256 amount
  );

  // Specify the UniSwapV2 or SushiSwap factory address
  // We may later change this to a centralise auth system where we can add factories in future dynamically
  constructor(address _factory, bytes32[] memory _cardanoDexes) {
    factory = _factory;
    cardanoDexes = _cardanoDexes;
    owner = msg.sender;
  }

  // Checks an LP token is a pair belonging to the factory
  function validatePair(IUniswapV2Pair pair) internal view returns (address, address) {
    address token0 = pair.token0();
    address token1 = pair.token1();
    address expectedPair = address(uint(keccak256(abi.encodePacked(
      hex'ff',
      factory,
      keccak256(abi.encodePacked(token0, token1)),
      hex'96e8ac4277198ff8b6f785478aa9a39f403cb768dd02cbee326c3e7da348845f'
    ))));
    require(address(pair) == expectedPair);
    return (token0, token1);
  }

  function validateDexName(bytes32 dexName) internal view {
    bool valid = false;
    for(uint i = 0; i < cardanoDexes.length; i++) {
      if(dexName == cardanoDexes[i]) {
        valid = true;
      }
    }
    require(valid);
  }

  function updateConfig(bytes32[] memory _cardanoDexes) public {
    require(msg.sender == owner);
    cardanoDexes = _cardanoDexes;
  }

  // Deposit all allowed tokens of a pair to a cardano address
  // Authenticates the pair, withdraws all the LP tokens and voids the result
  // Lastly, calls the event
  function deposit(IUniswapV2Pair pair, bytes32 dexName, bytes28 cardanoAddress) public {
    (address token0, address token1) = validatePair(pair);
    uint256 amount = pair.allowance(msg.sender, address(this)); // Get everything they'll give us
    require(amount > 0);
    pair.transferFrom(msg.sender, address(pair), amount); // Transfer it all to the pair
    // TODO: Should we store these tokens temporarily incase something goes wrong? or just persist cardano side until it works?
    (uint amount0, uint amount1) = pair.burn(address(0)); // Burn the liquidity tokens and void the underlying erc20s
    // Emit the event with given data
    validateDexName(dexName);

    emit Deposit(
      msg.sender,
      cardanoAddress,
      dexName,
      IERC20(token0).name(),
      IERC20(token1).name(),
      token0,
      token1,
      amount0,
      amount1
    );
  }

  // Accepts WEth allowance or direct Eth, voids and emits event
  function depositEth(bytes28 cardanoAddress) public payable {
    uint256 wethAmount = weth.allowance(msg.sender, address(this));
    uint256 ethAmount = msg.value;
    if (wethAmount > 0) {
      weth.transferFrom(msg.sender, address(0), wethAmount);
    }
    if (ethAmount > 0) {
      bool sent = address(0).send(msg.value);
      require(sent, "Failed to void ether");
    }
    uint256 amount = wethAmount + ethAmount;
    require(amount > 0);

    emit DepositEth(
      msg.sender,
      cardanoAddress,
      amount
    );
  }
}
