import Head from "next/head";
import React, { ReactNode } from "react";
import "../../styles/site.scss";

export function Layout({ children }: { children: ReactNode }) {
  return (
    <div>
      <Head>
        <title>Bish didyou?!?!!</title>
      </Head>
      {children}
      <a
        href="https://docs.google.com/spreadsheets/d/1nbqOF_xE_ANFzA-pxnDVVG2iH45E6khYVHrc36l5Opo/edit#gid=0"
        style={{ float: "right", marginRight: "50px" }}
        className="button"
      >
        Get it!
      </a>
    </div>
  );
}
