import React from "react";
import { fetchDelinquents, IDelinquent } from "../src/api";

interface IProps {
  delinquents: IDelinquent[];
}

const Delinquents = ({ delinquents }: IProps) => {
  if (delinquents.length === 0) {
    return <AllGood />;
  }

  return (
    <>
      <h2>Y'all Gotta Step up your game:</h2>

      {delinquents.map(({ name, delinquentActivities }) => (
        <div key={name}>
          {name}: {delinquentActivities}
        </div>
      ))}
    </>
  );
};

const AllGood = () => <h2>"Great Job 'err one</h2>;

const IndexPage = ({ delinquents }: IProps) => {
  return (
    <div>
      <Delinquents delinquents={delinquents} />
      <br />
      <br />
      <a href="https://docs.google.com/spreadsheets/d/1nbqOF_xE_ANFzA-pxnDVVG2iH45E6khYVHrc36l5Opo/edit#gid=0">
        Get it!
      </a>
    </div>
  );
};

IndexPage.getInitialProps = async (): Promise<IProps> => {
  const delinquents = await fetchDelinquents();
  return { delinquents };
};

export default IndexPage;
