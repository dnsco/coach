import React from "react";
import { fetchDelinquents, IApiPerson } from "../src/api";

interface IProps {
  people: IApiPerson[];
}

const IndexPage = ({ people }: IProps) => {
  return (
    <div>
      {people.map(({ name, activities }) => (
        <div key={name}>
          <h3>{name}</h3>
          {activities.map(({ title, isDelinquent }) => (
            <span key={name + title}>
              <h4 style={{ display: "inline" }}>{title}</h4>{" "}
              {isDelinquent ? "Step it up, queen." : "You Got it!"}
            </span>
          ))}
        </div>
      ))}
      <br />
      <br />
      <a href="https://docs.google.com/spreadsheets/d/1nbqOF_xE_ANFzA-pxnDVVG2iH45E6khYVHrc36l5Opo/edit#gid=0">
        Get it!
      </a>
    </div>
  );
};

IndexPage.getInitialProps = async (): Promise<IProps> => {
  const people = await fetchDelinquents();
  return { people };
};

export default IndexPage;
