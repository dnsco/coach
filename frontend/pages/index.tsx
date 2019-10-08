import Head from "next/head";
import React from "react";
import { fetchNewPeople, IApiNewPerson } from "../src/api";

interface IProps {
  people: IApiNewPerson[];
}

const NewPage = ({ people: [names, activities, ...days] }: IProps) => {
  return (
    <div>
      <Head>
        <title>Bish didyou?!?!!</title>
        <link
          rel="stylesheet"
          href="//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
        />
        <link
          rel="stylesheet"
          href="//cdnjs.cloudflare.com/ajax/libs/normalize/5.0.0/normalize.css"
        />
        <link
          rel="stylesheet"
          href="//cdnjs.cloudflare.com/ajax/libs/milligram/1.3.0/milligram.css"
        />
      </Head>
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

      <a
        href="https://docs.google.com/spreadsheets/d/1nbqOF_xE_ANFzA-pxnDVVG2iH45E6khYVHrc36l5Opo/edit#gid=0"
        style={{ float: "right", marginRight: "50px" }}
        className="button"
      >
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
