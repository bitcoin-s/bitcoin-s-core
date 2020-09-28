CREATE TABLE `r_values`
(
    `nonce`         VARCHAR(254) NOT NULL,
    `label`         VARCHAR(254) NOT NULL UNIQUE,
    `hd_purpose`    INTEGER      NOT NULL,
    `coin`          INTEGER      NOT NULL,
    `account_index` INTEGER      NOT NULL,
    `chain_type`    INTEGER      NOT NULL,
    `key_index`     INTEGER      NOT NULL,
    PRIMARY KEY (`nonce`)
);

CREATE TABLE `events`
(
    `nonce`           VARCHAR(254) NOT NULL,
    `label`           VARCHAR(254) NOT NULL UNIQUE,
    `num_outcomes`    INTEGER      NOT NULL,
    `signing_version` VARCHAR(254) NOT NULL,
    `attestation`     VARCHAR(254),
    CONSTRAINT `fk_label` FOREIGN KEY (`label`) REFERENCES `r_values` (`label`) on update NO ACTION on delete NO ACTION,
    PRIMARY KEY (`nonce`),
    CONSTRAINT `fk_nonce` FOREIGN KEY (`nonce`) REFERENCES `r_values` (`nonce`) on update NO ACTION on delete NO ACTION
);

CREATE TABLE `event_outcomes`
(
    `nonce`          VARCHAR(254) NOT NULL,
    `message`        VARCHAR(254) NOT NULL,
    `hashed_message` VARCHAR(254) NOT NULL,
    CONSTRAINT `fk_nonce` FOREIGN KEY (`nonce`) REFERENCES `events` (`nonce`) on update NO ACTION on delete NO ACTION
);
