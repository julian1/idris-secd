
// https://ethereum.stackexchange.com/questions/25839/how-to-make-transactions-using-private-key-in-web3

const EthereumTx = require('ethereumjs-tx')

var Web3 = require("web3")
var url = "https://api.myetherapi.com/rop"
web3 = new Web3(new Web3.providers.HttpProvider(url));



const nonce = web3.eth.getTransactionCount('0xbd4ea8d2935f59728b1de7c43168ba0bc2c7496e' );
console.log('nonce ' + nonce);

// send it through with the 10x higher has price and it's instant.
// gas used is the same which is expected. 
const gasPrice = web3.eth.gasPrice * 2;
console.log('network gasPrice ' + gasPrice.toString(10));  // ropsten 10000000000 == 10 gwei.
const gasPriceHex = web3.toHex(gasPrice);

const gasLimitHex = web3.toHex(300000);       // seems to need a bit more than 21000 mainnet.
                                              // because of our extra bit of data.
                                              // IMPORTANT - unit is gas (not ether) 

const privateKey = Buffer.from('aba2e1fda0fc622c4865558701b291f626f32cf0b136b1a0d0595f8e2689bf86', 'hex')

const txParams = {

  nonce: nonce,
 
  gasPrice: gasPriceHex,
  gasLimit: gasLimitHex,


  // to: '',     // no value, = contract creation 
  // data: '0x606060405260298060106000396000f37fEEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEE60005260206000f3', 

  to: '0x6ce1d1204509d925c11408956623E60dd20bD06D',
  data: '0x00',


  // EIP 155 chainId - mainnet: 1, ropsten: 3
  chainId: 3
}

const tx = new EthereumTx(txParams)
tx.sign(privateKey)

const serializedTx = tx.serialize()

stringTx = serializedTx.toString('hex');
console.log(stringTx);



web3.eth.sendRawTransaction('0x' + stringTx, function(err, hash) {
  if (!err) {
    console.log(hash);

    // should be able to use this....
    // but need to block...
    // console.log( web3.eth.getTransactionReceipt(hash) );
  }

  else
    console.log(err);
});


