package netwatch.manager.entities;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

// This class represents the "utilities" entity in the database.
// The Primary Key is the field "address".
// @author Antonio Scardace
// @version 1.0

@NoArgsConstructor
@AllArgsConstructor
@Data
@Entity
@Table(name = "utilities")
public class Utility implements Serializable {

    @Id
    @Column(name = "address", nullable = false)
    private String address;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "type", nullable = false)
    private String type;
}