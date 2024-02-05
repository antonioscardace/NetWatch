package netwatch.manager.entities;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

// This class represents the "contacts" entity in the database.
// The Primary Key is the field "value".
// @author Antonio Scardace
// @version 1.0

@NoArgsConstructor
@AllArgsConstructor
@Data
@Entity
@Table(name = "contacts")
public class Contact implements Serializable {

    @Id
    @Column(name = "value", nullable = false)
    private String value;

    @Column(name = "type", nullable = false)
    private String type;
}