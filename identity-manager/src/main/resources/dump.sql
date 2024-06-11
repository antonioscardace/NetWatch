DROP DATABASE IF EXISTS `auth`;
CREATE DATABASE `auth`;
USE `auth`;

--
-- Table structure for table `roles`
--

CREATE TABLE `roles` (
    `name` VARCHAR(16) NOT NULL,
    `description` VARCHAR(128) NOT NULL,
    PRIMARY KEY(`name`)
);

--
-- Dumping data for table `roles`
--

INSERT INTO `roles` VALUES (
    'admin',
    'System Administrator'
);

--
-- Table structure for table `users`
--

CREATE TABLE `users` (
    `username` VARCHAR(32) NOT NULL,
    `password` VARCHAR(128) NOT NULL,
    `role` VARCHAR(16) NOT NULL,
    PRIMARY KEY(`username`),
    FOREIGN KEY(`role`) REFERENCES roles(`name`) ON DELETE CASCADE
);

--
-- Dumping data for table `roles`
-- username: administrator
-- password: password
-- role: admin
--

INSERT INTO `users` VALUES (
    'administrator',
    '$s0$f0801$CgsVsvYsQTPYFtZSVK8FBA==$OvUHsj8mR5rZa159H5r7IJfL57pWA3DgiUt0818sM60=',
    'admin'
);

--
-- Table structure for table `permissions`
--

CREATE TABLE `permissions` (
    `role` VARCHAR(16) NOT NULL,
    `method` VARCHAR(16) NOT NULL,
    `object` VARCHAR(32) NOT NULL,
    PRIMARY KEY(`role`, `method`, `object`),
    FOREIGN KEY(`role`) REFERENCES roles(`name`) ON DELETE CASCADE
);

--
-- Dumping data for table `permissions`
-- Data are just for the admin
--

INSERT INTO `permissions` VALUES ( -- Authorization for "/users/" enpoint
    'admin',
    'GET',
    '/users/'
), (
    'admin',
    'GET',
    '/users/check'
), (
    'admin',
    'PUT',
    '/users/'
), (
    'admin',
    'DELETE',
    '/users/'
);

INSERT INTO `permissions` VALUES ( -- Authorization for "/roles/" enpoint
    'admin',
    'GET',
    '/roles/'
), (
    'admin',
    'POST',
    '/roles/'
), (
    'admin',
    'PUT',
    '/roles/'
), (
    'admin',
    'DELETE',
    '/roles/'
);

INSERT INTO `permissions` VALUES ( -- Authorization for "/authorization/" enpoint
    'admin',
    'GET',
    '/authorization/'
), (
    'admin',
    'GET',
    '/authorization/check'
), (
    'admin',
    'GET',
    '/authorization/filter'
), (
    'admin',
    'POST',
    '/authorization/'
), (
    'admin',
    'DELETE',
    '/authorization/'
);

INSERT INTO `permissions` VALUES ( -- Authorization for "/contacts/" enpoint
    'admin',
    'GET',
    '/contacts/'
), (
    'admin',
    'GET',
    '/contacts/check'
), (
    'admin',
    'POST',
    '/contacts/'
), (
    'admin',
    'DELETE',
    '/contacts/'
);

INSERT INTO `permissions` VALUES ( -- Authorization for "/utilities/" enpoint
    'admin',
    'GET',
    '/utilities/'
), (
    'admin',
    'GET',
    '/utilities/check'
), (
    'admin',
    'POST',
    '/utilities/'
), (
    'admin',
    'PUT',
    '/utilities/'
), (
    'admin',
    'DELETE',
    '/utilities/'
);

INSERT INTO `permissions` VALUES ( -- Authorization for "/observations/" enpoint
    'admin',
    'GET',
    '/observations/'
), (
    'admin',
    'GET',
    '/observations/check'
), (
    'admin',
    'GET',
    '/observations/filter/admin'
), (
    'admin',
    'POST',
    '/observations/admin'
), (
    'admin',
    'DELETE',
    '/observations/admin'
);