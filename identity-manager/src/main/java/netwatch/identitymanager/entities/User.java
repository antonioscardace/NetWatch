package netwatch.identitymanager.entities;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

// This class represents the "users" entity in the database.
// The Primary Key is the field "username".
// The "role" field is a Many-to-One relationship to the "name" column in the {@link Role} entity.
// @author Antonio Scardace
// @version 1.0

@NoArgsConstructor
@AllArgsConstructor
@Data
@Entity
@Table(name = "users")
public class User {

    @Id
    @Column(name = "username", nullable = false)
    private String username;

    @Column(name = "password", nullable = false)
    private String password;

    @ManyToOne
    @JoinColumn(name = "role", referencedColumnName = "name", nullable = false)
    private Role role;
}