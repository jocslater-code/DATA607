-- Users Table
CREATE TABLE users (
    user_id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL
);

-- Movies Table
CREATE TABLE movies (
    movie_id SERIAL PRIMARY KEY,
    title VARCHAR(255) NOT NULL
);

-- Rating Table 
CREATE TABLE ratings (
    user_id INT REFERENCES users(user_id) ON DELETE CASCADE,
    movie_id INT REFERENCES movies(movie_id) ON DELETE CASCADE,
    rating NUMERIC(3, 1) NOT NULL,
    PRIMARY KEY (user_id, movie_id),
    CONSTRAINT chk_rating CHECK (rating BETWEEN 0.0 AND 5.0)
);

--Insert Users
INSERT INTO users (name) VALUES 
('Heidi Smith'),
('Michael Pento'),
('Cody Slater'),
('Jocelyn Slater')
('Jacob OBrien');

-- Insert Movies
INSERT INTO movies (title) VALUES 
('Spiderman Brand New Day'),
('The Odyssey'),
('Toy Story 5'),
('Project Hail Mary'),
('Superman'), 
('Goat');

-- Insert Ratings
INSERT INTO ratings (user_id, movie_id, rating) VALUES 
(1, 3, 4.3), -- Heidi rated Toy Story 5 4.3
(1, 6, 4.5), -- Heidi rated Goat 4.5
(2, 3, 4.3), -- Michael rated Toy Story 5 a 4.3
(2, 6, 4.0), -- Michael rated Goat 4.0
(2, 4, 4.8), -- Michael rated Project Hail Mary 4.8
(3, 2, 4.5), -- Cody rated The Odyssey 4.5
(3, 3, 3.9), -- Cody rated Toy Story 5 3.9
(3, 4, 3.7), -- Cody rated Project Hail Mary 3.7
(3, 5, 4.2), -- Cody rated Superman 4.2
(3, 6, 3.5), -- Cody rated Goat 3.5
(4, 3, 4.2), -- Jocelyn rated Toy Story 5 4.2
(4, 4, 4.8), -- Jocelyn rated Project Hail Mary 4.8
(4, 5, 4.7), -- Jocelyn rated Superman 4.7
(4, 6, 4.0), -- Jocelyn rated Goat 4.0
(5, 1, 4.6), -- Jacob rated Spideman 4.6
(5, 2, 5.0), -- Jacob rated The Odyssey 5.0
(5, 3, 4.0), -- Jacob rated Toy Story 5 4.0
(5, 4, 4.3), -- Jacob rated Project Hail Mary 4.3
(5, 5, 4.5), -- Jacob rated Superman 4.5
(5, 6, 3.3); -- Jacob rated Goat 3.3



