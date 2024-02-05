DROP DATABASE IF EXISTS `manager`;
CREATE DATABASE `manager`;
USE `manager`;

--
-- Table structure for table `contacts`
--

CREATE TABLE `contacts` (
    `value` VARCHAR(64) NOT NULL,
    `type` VARCHAR(16) NOT NULL,
    PRIMARY KEY(`value`)
);

--
-- Table structure for table `utilities`
--

CREATE TABLE `utilities` (
    `address` VARCHAR(32) NOT NULL,
    `name` VARCHAR(32) NOT NULL,
    `type` VARCHAR(8) NOT NULL,
    PRIMARY KEY(`address`)
);

--
-- Table structure for table `observes`
--

CREATE TABLE `observes` (
    `address` VARCHAR(32) NOT NULL,
    `contact` VARCHAR(64) NOT NULL,
    `environment` VARCHAR(16) NOT NULL,
    PRIMARY KEY(`address`, `contact`),
    FOREIGN KEY(`address`) REFERENCES utilities(`address`) ON DELETE CASCADE,
    FOREIGN KEY(`contact`) REFERENCES contacts(`value`) ON DELETE CASCADE
);