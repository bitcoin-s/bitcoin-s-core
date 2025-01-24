---
id: version-1.9.10-ui-setup
title: Installing the DLC Wallet UI
original_id: ui-setup
---

# Easy setup

We have desktop installers for mac, windows and linux. This requires you to be signed into a github account to download.

The link to the installers is [suredbits.com/bitcoin-s](https://suredbits.com/bitcoin-s)

You can download the appropriate installer for your machine 

![Example banner](/img/installers.png)

The downside of this setup is it uses an old UI that we are working to get rid of.
This UI is missing new features that drastically improve UX. 

Once you download and install the wallet, deposit 100,000 sats and [find an event you want to bet on](https://oracle.suredbits.com/)!

The wallet will take roughly 20-30 minutes to synchronize with the bitcoin network. If you deposit funds before
the synchronization finishes, the funds may not show up right away. This is expected. They will show up when the sync is done.

For more information about building a DLC with this UI, checkout this youtube tutorial I recorded: 

https://youtu.be/oR0I0aHxNMM?t=219

# Advanced setup

This requires command line skills.


First, clone the bitcoin-s-ts repository by running 

```
git clone https://github.com/bitcoin-s/bitcoin-s-ts.git
```

### Setup

(If on a M1 Mac, go to the Generate M1 Mac server section below before continuing)

You must have npm, if you don't then run `brew install node` 
Next, navigate to the `bitcoin-s-ts` directory and in it run 

```
./build-wallet-electron.sh
```
This will download the `bitcoin-s-server.zip` file and set up Suredbits Wallets dependencies. 

Now, if you wish to run the Suredbits Wallet application in dev mode then run 

```
cd wallet-electron-ts && npm run start
```

### Install desktop application (Optional)

If you would like to use the desktop application then, search for the Suredbits Wallet application in finder, windows explorer, ect ... double click on the icon and complete the installation 

Once you open the Suredbits Wallet application you should see the following screen
 
![Alt text](/img/Screenshot%20from%202022-03-11%2011-20-17.png)

The password is `none`, enter that and you should see the wallet!

![Alt text](/img/Screenshot%20from%202022-03-11%2011-21-47.png)
 
Deposit 100,000 sats and [find an event you want to bet on](https://oracle.suredbits.com/)!

The wallet will take roughly 20-30 minutes to synchronize with the bitcoin network. If you deposit funds before
the synchronization finishes, the funds may not show up right away. This is expected. They will show up when the sync is done.

After the synchronization is done, you should be good to do a DLC! :tada:

### Generate M1 Mac server

If you are on an M1 Mac, we currently do not offer a `bitcoin-s-sever.zip` file on the website, so the setup requires a few more steps

First, you will need a bitcoin-s node on your machine. If you don't already have one, follow the  instructions here https://bitcoin-s.org/docs/getting-setup 

Next, to generate your the `bitcoin-s-server.zip` file, go into your bitcoin-s directory and run

```
sbt appServer/universal:packageBin
```
Then navigate to the bitcoin-s sub-directory containing the .zip file by running 

```
cd app/server/target/universal
```

Now we will copy this file over to `bitcoin-s-ts/wallet-electron-ts` by running 

```
cp <the zip file> ~/bitcoin-s-ts/wallet-electron-ts
```
You now can return to the Setup section

