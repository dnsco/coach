import React from "react";
import { IApiActivity, IApiPerson } from "../api";
import { FComponent } from "./lib";

interface IPersonProps {
  person: IApiPerson;
}

export const Person: FComponent<IPersonProps> = ({
  person: { name, activities }
}) => (
  <div className="person">
    <h3>{name}</h3>
    {activities.map(a => (
      <Activity key={name + a.title} activity={a} />
    ))}
  </div>
);

function Activity({ activity }: { activity: IApiActivity }) {
  const { title, events } = activity;
  return (
    <div className="activity">
      <h4>{title}</h4>
      {events.map(([_, s], i) => (
        <div key={i + s} className="event-text">
          {s}
        </div>
      ))}
    </div>
  );
}
