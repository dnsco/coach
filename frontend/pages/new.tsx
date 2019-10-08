import React from "react";
import { fetchNewPeople, IApiNewPerson } from "../src/api";

interface IProps {
  people: IApiNewPerson[];
}

const NewPage = ({ people: [names, activities, ...days] }: IProps) => {
  return (
    <div>
      <table>
        <tr>
          {names.map((name, i) => (
            <th key={name}>
              {name} {activities[i]}
            </th>
          ))}
        </tr>

        {days.map((events, i) => (
          <tr key={i}>
            {events.map((e, j) => (
              <td key={j}>{e}</td>
            ))}
          </tr>
        ))}
      </table>

      <a href="https://docs.google.com/spreadsheets/d/1nbqOF_xE_ANFzA-pxnDVVG2iH45E6khYVHrc36l5Opo/edit#gid=0">
        Get it!
      </a>
    </div>
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
