/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require("react");

const CompLibrary = require("../../core/CompLibrary.js");

const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

const Logo = props => (
  <div className="projectLogo">
    <img src={props.img_src} alt="Project Logo" />
  </div>
);

// cribbed from Bloop site, https://github.com/scalacenter/bloop/blob/6b5384241d1bba4143315e66f668876d65a2e34f/website/pages/en/index.js#L92
const Hero = ({ siteConfig }) => (
  <div className="hero">
    <div className="hero__container">
      <Logo img_src={`${siteConfig.baseUrl}img/bitcoin-s-logo.png`} />
      <h1>{siteConfig.tagline}</h1>
      <p style={{ margin: "1rem 0 1.5rem 0" }}>lorem ipsum</p>
      <a
        className="github-button"
        href={siteConfig.repoUrl}
        data-size="large"
        data-show-count="true"
        aria-label="Star bitcoin-s/bitcoin-s on GitHub"
      >
        Star
      </a>
    </div>
  </div>
);

class HomeSplash extends React.Component {
  render() {
    const { siteConfig, language = "" } = this.props;
    const { baseUrl, docsUrl } = siteConfig;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ""}`;
    const langPart = `${language ? `${language}/` : ""}`;
    const docUrl = doc => `${baseUrl}${docsPart}${langPart}${doc}`;

    const SplashContainer = props => (
      <div className="homeContainer">
        <div className="homeSplashFade">
          <div className="wrapper homeWrapper">{props.children}</div>
        </div>
      </div>
    );

    const ProjectTitle = () => (
      <h2 className="projectTitle">
        <small>{siteConfig.tagline}</small>
      </h2>
    );

    const PromoSection = props => (
      <div className="section promoSection">
        <div className="promoRow">
          <div className="pluginRowBlock">{props.children}</div>
        </div>
      </div>
    );

    const Button = props => (
      <div className="pluginWrapper buttonWrapper">
        <a className="button" href={props.href} target={props.target}>
          {props.children}
        </a>
      </div>
    );

    return (
      <SplashContainer>
        <div className="inner">
          <ProjectTitle siteConfig={siteConfig} />
          <PromoSection>
            {/*
            <Button href="#try">Try It Out</Button>
            <Button href={docUrl("doc1.html")}>Example Link</Button>
            <Button href={docUrl("doc2.html")}>Example Link 2</Button> */}
          </PromoSection>
        </div>
      </SplashContainer>
    );
  }
}

class Index extends React.Component {
  render() {
    const { config: siteConfig, language = "" } = this.props;
    const { baseUrl } = siteConfig;

    const Block = props => (
      <Container
        padding={["bottom", "top"]}
        id={props.id}
        background={props.background}
      >
        <GridBlock
          align="center"
          contents={props.children}
          layout={props.layout}
        />
      </Container>
    );

    const FeatureCallout = () => (
      <div
        className="productShowcaseSection paddingBottom"
        style={{ textAlign: "center" }}
      >
        <h2>Feature Callout</h2>
        <MarkdownBlock>These are features of this project</MarkdownBlock>
      </div>
    );

    const TryOut = () => (
      <Block id="try">
        {[
          {
            content: [
              "Use our RPC clients for `bitcoind`/Bitcoin Core and Eclair, and get powerful",
              "static typing baked into your RPC calls. All returned values you get from `bitcoind`",
              "and Eclair are converted into native Bitcoin/Lightning data structures for you.",
              "Is that raw hex string you've been passing around a transaction or a Lightning invoice?",
              "With Bitcoin-S you get both confidence in your code _and_ powerful methods available",
              "on your data"
            ].join(" "),
            image: `${baseUrl}img/undraw_code_review.svg`,
            imageAlign: "left",
            title: "Super-powered RPC clients"
          }
        ]}
      </Block>
    );

    const Description = () => (
      <Block background="dark">
        {[
          {
            content:
              "This is another description of how this project is useful",
            image: `${baseUrl}img/undraw_note_list.svg`,
            imageAlign: "right",
            title: "Description"
          }
        ]}
      </Block>
    );

    const LearnHow = () => (
      <Block background="light">
        {[
          {
            content: [
              "We provide solid APIs for constructing and signing transactions.",
              "From small-scale 1-in 2-out transactions, to custom logic powering exchange withdrawals, we've got you covered.",
              "Check out our [`TxBuilder` example](docs/txbuilder) to see how."
            ].join(" "),
            image: `${baseUrl}img/undraw_youtube_tutorial.svg`,
            imageAlign: "right",
            title: "Construct and sign bitcoin transactions"
          }
        ]}
      </Block>
    );

    const Features = () => (
      <Block layout="fourColumn">
        {[
          {
            content:
              "Code with confidence, knowing your data won't change under you",
            image: `${baseUrl}img/undraw_react.svg`,
            imageAlign: "top",
            title: "Immutable data structures"
          },
          {
            content: [
              "Get the compiler to work for you, ensuring your logic covers all cases.",
              "Modelling your application with mathematically founded types enables greater confidence in the correctness of your code"
            ].join(" "),
            image: `${baseUrl}img/undraw_operating_system.svg`,
            imageAlign: "top",
            title: "Algebraic data types"
          }
        ]}
      </Block>
    );

    const Showcase = () => {
      if ((siteConfig.users || []).length === 0) {
        return null;
      }

      const showcase = siteConfig.users
        .filter(user => user.pinned)
        .map(user => (
          <a href={user.infoLink} key={user.infoLink}>
            <img src={user.image} alt={user.caption} title={user.caption} />
          </a>
        ));

      const pageUrl = page => baseUrl + (language ? `${language}/` : "") + page;

      return (
        <div className="productShowcaseSection paddingBottom">
          <h2>Who is using Bitcoin-S?</h2>
          <p>
            Bitcoin-S is used in production applications, by both small and
            large companies
          </p>
          <div className="logos">{showcase}</div>
          <div className="more-users">
            <a className="button" href={pageUrl("users.html")}>
              Read more
            </a>
          </div>
        </div>
      );
    };

    return (
      <div>
        <Hero siteConfig={siteConfig} />
        <div className="mainContainer">
          <Features />
          <FeatureCallout />
          <LearnHow />
          <TryOut />
          <Description />
          <Showcase />
        </div>
      </div>
    );
  }
}

module.exports = Index;
