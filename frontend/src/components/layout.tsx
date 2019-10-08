import Head from "next/head";
import React, { ReactNode } from "react";

export function Layout({ children }: { children: ReactNode }) {
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
