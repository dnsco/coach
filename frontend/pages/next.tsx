import React from "react";
import { fetchNewPeople, IApiNewPerson } from "../src/api";
import { Layout } from "../src/components/layout";

interface IProps {
  people: IApiNewPerson[];
}

const NewPage = ({ people: [names, activities, ...days] }: IProps) => {
  return (
    <Layout>
      <table>
        <tr>
          {names.map((name, i) => (
            <th key={name} style={{ padding: "15px 20px", minWidth: "360px" }}>
              <h4 style={{ margin: "0px" }}>{name}</h4>{" "}
              <h5 style={{ margin: "0px", fontWeight: "bold" }}>
                {activities[i]}
              </h5>
            </th>
          ))}
        </tr>

        {days.map((events, i) => (
          <tr key={i}>
            {events.map((e, j) => (
              <td key={j} style={{ padding: "10px 20px" }}>
                {e}
              </td>
            ))}
          </tr>
        ))}
      </table>
    </Layout>
  );
};

// <span key={name + title}>
//   <h4 style={{display: "inline"}}>{title}</h4>{" "}
// {isDelinquent ? "Step it up, queen." : "You Got it!"}
// </span>
//

NewPage.getInitialProps = async (): Promise<IProps> => {
  const people = await fetchNewPeople();
  return { people };
};

export default NewPage;
