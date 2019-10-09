import React from "react";
import { fetchDelinquents, IApiActivity, IApiPerson } from "../src/api";
import { Layout } from "../src/components/layout";
import { FComponent } from "../src/components/lib";

interface IProps {
  people: IApiPerson[];
}

function Activity({ activity }: { activity: IApiActivity }) {
  const { title, events } = activity;
  return (
    <table>
      <thead>
        <tr>
          <th>{title}</th>
        </tr>
      </thead>
      <tbody>
        {events.map(([_, s], i) => (
          <tr key={i + s}>
            <td className="event-text">{s}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

interface IPersonProps {
  person: IApiPerson;
}

const Person: FComponent<IPersonProps> = ({ person: { name, activities } }) => (
  <div className="person">
    <h3>{name}</h3>
    {activities.map(a => (
      <Activity key={name + a.title} activity={a} />
    ))}
  </div>
);

const IndexPage = ({ people }: IProps) => {
  return (
    <Layout>
      <div className="container">
        <div className="row">
          {people.map((person: IApiPerson) => (
            <Person key={person.name} person={person} />
          ))}
        </div>
      </div>
    </Layout>
  );
};

IndexPage.getInitialProps = async (): Promise<IProps> => {
  const people = await fetchDelinquents();
  people.reverse();

  return { people };
};

export default IndexPage;
