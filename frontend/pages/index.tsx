import React from "react";
import { fetchDelinquents, IApiPerson } from "../src/api";
import { Layout } from "../src/components/layout";
import { Person } from "../src/components/person";

interface IProps {
  people: IApiPerson[];
  sheetUrl: string;
}

const IndexPage = ({ people, sheetUrl }: IProps) => {
  return (
    <Layout>
      <div className="container">
        <div className="row">
          {people.map((person: IApiPerson) => (
            <Person key={person.name} person={person} />
          ))}
        </div>
        <a href={sheetUrl} className="sheet-link">
          Get it!
        </a>
      </div>
    </Layout>
  );
};

IndexPage.getInitialProps = async (): Promise<IProps> => {
  const people = await fetchDelinquents();
  people.reverse();

  return { people, sheetUrl: SHEET_URL };
};

const SHEET_URL =
  "https://docs.google.com/spreadsheets/d/1nbqOF_xE_ANFzA-pxnDVVG2iH45E6khYVHrc36l5Opo/edit#gid=0";

export default IndexPage;
