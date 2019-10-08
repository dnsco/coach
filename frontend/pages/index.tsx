import React from "react";
import { fetchDelinquents, IApiActivity, IApiPerson } from "../src/api";
import { Layout } from "../src/components/layout";
import { FComponent } from "../src/components/lib";

interface IProps {
  people: IApiPerson[];
}

function Activity({ activity }: { activity: IApiActivity }) {
  const { title, isDelinquent, events } = activity;
  return (
    <span>
      <table>
        <thead>
          <tr>
            <th>
              {title}
              <div className="small">
                {isDelinquent ? "Step it up, queen." : "You Got it!"}
              </div>
            </th>
          </tr>
        </thead>
        <tbody>
          {events.map(([_, s], i) => (
            <tr key={i + s}>
              <td
                style={{
                  height: "50px",
                  textOverflow: "ellipsis",
                  whiteSpace: "nowrap"
                }}
              >
                {s}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </span>
  );
}

interface IPersonProps {
  person: IApiPerson;
}

const Person: FComponent<IPersonProps> = ({ person: { name, activities } }) => (
  <div className="column column-33" style={{overflow:"hidden"}}>
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
